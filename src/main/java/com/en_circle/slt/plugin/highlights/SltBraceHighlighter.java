package com.en_circle.slt.plugin.highlights;

import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.lisp.psi.LispTypes;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlighterPassFactory;
import com.intellij.codeInsight.highlighting.BraceHighlightingHandler;
import com.intellij.codeInsight.highlighting.BraceMatchingUtil;
import com.intellij.codeInsight.highlighting.BraceMatchingUtil.BraceHighlightingAndNavigationContext;
import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.injected.editor.EditorWindow;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.event.*;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.openapi.editor.markup.HighlighterLayer;
import com.intellij.openapi.editor.markup.HighlighterTargetArea;
import com.intellij.openapi.editor.markup.RangeHighlighter;
import com.intellij.openapi.extensions.ExtensionPointUtil;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManagerEvent;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.fileTypes.BinaryFileTypeDecompilers;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupActivity;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiBinaryFile;
import com.intellij.psi.PsiCompiledFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.Alarm;
import com.intellij.util.containers.Stack;
import com.intellij.util.text.CharArrayUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

// Copied from IJ source to implement highlight WITHOUT smart insertion!

public class SltBraceHighlighter implements StartupActivity.DumbAware {
    private static final Key<List<RangeHighlighter>> BRACE_HIGHLIGHTERS_IN_EDITOR_VIEW_KEY = Key.create("BraceHighlighter.BRACE_HIGHLIGHTERS_IN_EDITOR_VIEW_KEY");
    private static final Key<RangeHighlighter> LINE_MARKER_IN_EDITOR_KEY = Key.create("BraceHighlighter.LINE_MARKER_IN_EDITOR_KEY");

    private final Alarm alarm = new Alarm();

    @Override
    public void runActivity(@NotNull Project project) {
        if (ApplicationManager.getApplication().isHeadlessEnvironment() && !ApplicationManager.getApplication().isUnitTestMode() ||
                !IdentifierHighlighterPassFactory.isEnabled()) {
            return;
        }

        Disposable activityDisposable = ExtensionPointUtil.createExtensionDisposable(this, StartupActivity.POST_STARTUP_ACTIVITY);
        // lul they have this in code with warning...
        Disposer.register(project, activityDisposable);
        registerListeners(project, activityDisposable);
    }

    private void registerListeners(@NotNull Project project, @NotNull Disposable parentDisposable) {
        EditorEventMulticaster eventMulticaster = EditorFactory.getInstance().getEventMulticaster();

        eventMulticaster.addCaretListener(new CaretListener() {
            @Override
            public void caretPositionChanged(@NotNull CaretEvent e) {
                if (e.getCaret() != e.getEditor().getCaretModel().getPrimaryCaret()) return;
                onCaretUpdate(e.getEditor(), project);
            }

            @Override
            public void caretAdded(@NotNull CaretEvent e) {
                if (e.getCaret() != e.getEditor().getCaretModel().getPrimaryCaret()) return;
                onCaretUpdate(e.getEditor(), project);
            }

            @Override
            public void caretRemoved(@NotNull CaretEvent e) {
                onCaretUpdate(e.getEditor(), project);
            }
        }, parentDisposable);

        SelectionListener selectionListener = new SelectionListener() {
            @Override
            public void selectionChanged(@NotNull SelectionEvent e) {
                alarm.cancelAllRequests();
                Editor editor = e.getEditor();
                if (editor.getProject() != project) {
                    return;
                }
                if (isNotCLFile(editor)) {
                    return;
                }

                TextRange oldRange = e.getOldRange();
                TextRange newRange = e.getNewRange();
                if (oldRange != null && newRange != null && oldRange.isEmpty() == newRange.isEmpty()) {
                    return;
                }
                updateHighlighted(project, editor);
            }
        };
        eventMulticaster.addSelectionListener(selectionListener, parentDisposable);

        DocumentListener documentListener = new DocumentListener() {
            @Override
            public void documentChanged(@NotNull DocumentEvent e) {
                alarm.cancelAllRequests();
                EditorFactory.getInstance().editors(e.getDocument(), project).forEach(editor -> updateHighlighted(project, editor));
            }
        };
        eventMulticaster.addDocumentListener(documentListener, parentDisposable);

        project.getMessageBus().connect(parentDisposable)
                .subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new FileEditorManagerListener() {
                    @Override
                    public void selectionChanged(@NotNull FileEditorManagerEvent e) {
                        alarm.cancelAllRequests();
                        FileEditor oldEditor = e.getOldEditor();
                        if (oldEditor instanceof TextEditor) {
                            clearBraces(((TextEditor) oldEditor).getEditor());
                        }
                        FileEditor newEditor = e.getNewEditor();
                        if (newEditor instanceof TextEditor) {
                            updateHighlighted(project, ((TextEditor) newEditor).getEditor());
                        }
                    }
                });
    }

    private boolean isNotCLFile(Editor editor) {
        PsiFile file = PsiDocumentManager.getInstance(Objects.requireNonNull(editor.getProject())).getPsiFile(editor.getDocument());
        return file == null || !file.getFileType().equals(SltCommonLispFileType.INSTANCE);
    }

    private void clearBraces(Editor editor) {
        List<RangeHighlighter> highlighters = getHighlightersList(editor);
        for (RangeHighlighter highlighter : highlighters) {
            highlighter.dispose();
        }
        highlighters.clear();
    }

    private void updateHighlighted(Project project, Editor editor) {
        ApplicationManager.getApplication().assertIsDispatchThread();
        if (editor.getDocument().isInBulkUpdate()) {
            return;
        }

        clearBraces(editor);

        PsiFile psiFile = PsiUtilBase.getPsiFileInEditor(editor, project);
        if (psiFile == null) return;
        if (psiFile instanceof PsiCompiledFile) {
            psiFile = ((PsiCompiledFile) psiFile).getDecompiledPsiFile();
        }
        if (psiFile instanceof PsiBinaryFile && BinaryFileTypeDecompilers.getInstance().forFileType(psiFile.getFileType()) == null) {
            return;
        }

        if (editor instanceof EditorEx) {
            removeLineMarkers((EditorEx) editor);
        }

        if (editor.getSelectionModel().hasSelection()) return;

        if (editor.getSoftWrapModel().isInsideOrBeforeSoftWrap(editor.getCaretModel().getVisualPosition())) return;

        TemplateState state = TemplateManagerImpl.getTemplateState(editor);
        if (state != null && !state.isFinished()) return;

        int offset = editor.getCaretModel().getOffset();
        CharSequence chars = editor.getDocument().getCharsSequence();

        alarm.cancelAllRequests();
        BraceHighlightingAndNavigationContext context = computeHighlightingAndNavigationContext(editor, psiFile, offset);

        if (context != null) {
            doHighlight(editor, project, psiFile, context.currentBraceOffset, context.isCaretAfterBrace);
            offset = context.currentBraceOffset;
        } else if (offset > 0 && offset < chars.length()) {
            // There is a possible case that there are paired braces nearby the caret position and the document contains only white
            // space symbols between them. We want to highlight such braces as well.
            // Example:
            //     public void test() { <caret>
            //     }
            char c = chars.charAt(offset);
            boolean searchForward = c != '\n';

            // Try to find matched brace backwards.
            int backwardNonSpaceEndOffset = CharArrayUtil.shiftBackward(chars, offset - 1, "\t ") + 1;
            if (backwardNonSpaceEndOffset > 0 && backwardNonSpaceEndOffset < offset) {
                context = computeHighlightingAndNavigationContext(editor, psiFile, backwardNonSpaceEndOffset);
                if (context != null) {
                    doHighlight(editor, project, psiFile, context.currentBraceOffset, true);
                    offset = context.currentBraceOffset;
                    searchForward = false;
                }
            }

            // Try to find matched brace forward.
            if (searchForward) {
                int nextNonSpaceCharOffset = CharArrayUtil.shiftForward(chars, offset, "\t ");
                if (nextNonSpaceCharOffset > offset) {
                    context = computeHighlightingAndNavigationContext(editor, psiFile, nextNonSpaceCharOffset);
                    if (context != null) {
                        doHighlight(editor, project, psiFile, context.currentBraceOffset, true);
                        offset = context.currentBraceOffset;
                    }
                }
            }
        }
    }

    private void doHighlight(Editor editor, Project project, PsiFile psiFile, int offset, boolean isAdjustedPosition) {
        if (editor.getFoldingModel().isOffsetCollapsed(offset)) return;
        Document document = editor.getDocument();

        HighlighterIterator iterator = BraceHighlightingHandler.getLazyParsableHighlighterIfAny(project, editor, psiFile).createIterator(offset);
        CharSequence chars = document.getCharsSequence();

        FileType fileType = getFileTypeByOffset(psiFile, offset);

        if (isLBraceToken(iterator, chars, fileType)) {
            highlightLeftBrace(editor, iterator, false, fileType, document);

            if (offset > 0 && !isAdjustedPosition && !editor.getSettings().isBlockCursor()) {
                iterator = BraceHighlightingHandler.getLazyParsableHighlighterIfAny(project, editor, psiFile).createIterator(offset - 1);
                if (isRBraceToken(iterator, chars, fileType)) {
                    highlightRightBrace(editor, iterator, fileType, document);
                }
            }
        } else if (isRBraceToken(iterator, chars, fileType)) {
            highlightRightBrace(editor, iterator, fileType, document);
        }
    }

    private void highlightRightBrace(Editor editor, @NotNull HighlighterIterator iterator, @NotNull FileType fileType, Document document) {
        TextRange brace1 = TextRange.create(iterator.getStart(), iterator.getEnd());

        boolean matched = matchBrace(document.getCharsSequence(), fileType, iterator, false);

        TextRange brace2 = iterator.atEnd() ? null : TextRange.create(iterator.getStart(), iterator.getEnd());

        highlightBraces(editor, brace2, brace1, matched, false, fileType);
    }

    private void highlightLeftBrace(Editor editor, @NotNull HighlighterIterator iterator, boolean scopeHighlighting, @NotNull FileType fileType, Document document) {
        TextRange brace1Start = TextRange.create(iterator.getStart(), iterator.getEnd());
        boolean matched = matchBrace(document.getCharsSequence(), fileType, iterator, true);

        TextRange brace2End = iterator.atEnd() ? null : TextRange.create(iterator.getStart(), iterator.getEnd());

        highlightBraces(editor, brace1Start, brace2End, matched, scopeHighlighting, fileType);
    }

    private void highlightBraces(Editor editor, @Nullable TextRange lBrace,
                                 @Nullable TextRange rBrace, boolean matched, boolean scopeHighlighting, @NotNull FileType fileType) {
        if (!matched && fileType == FileTypes.PLAIN_TEXT) {
            return;
        }

        if (rBrace != null && !scopeHighlighting) {
            highlightBrace(editor, rBrace, matched);
        }

        if (lBrace != null && !scopeHighlighting) {
            highlightBrace(editor, lBrace, matched);
        }
    }

    private void highlightBrace(Editor editor, @NotNull TextRange braceRange, boolean matched) {
        TextAttributesKey attributesKey = matched ? CodeInsightColors.MATCHED_BRACE_ATTRIBUTES : CodeInsightColors.UNMATCHED_BRACE_ATTRIBUTES;
        RangeHighlighter rbraceHighlighter =
                editor.getMarkupModel().addRangeHighlighter(attributesKey, braceRange.getStartOffset(), braceRange.getEndOffset(),
                        HighlighterLayer.LAST + 1, HighlighterTargetArea.EXACT_RANGE);
        rbraceHighlighter.setGreedyToLeft(false);
        rbraceHighlighter.setGreedyToRight(false);
        registerHighlighter(editor, rbraceHighlighter);
    }

    private void registerHighlighter(Editor editor, @NotNull RangeHighlighter highlighter) {
        getHighlightersList(editor).add(highlighter);
    }

    @NotNull
    private List<RangeHighlighter> getHighlightersList(Editor myEditor) {
        // braces are highlighted across the whole editor, not in each injected editor separately
        Editor editor = myEditor instanceof EditorWindow ? ((EditorWindow) myEditor).getDelegate() : myEditor;
        List<RangeHighlighter> highlighters = editor.getUserData(BRACE_HIGHLIGHTERS_IN_EDITOR_VIEW_KEY);
        if (highlighters == null) {
            highlighters = new ArrayList<>();
            editor.putUserData(BRACE_HIGHLIGHTERS_IN_EDITOR_VIEW_KEY, highlighters);
        }
        return highlighters;
    }

    @NotNull
    private static FileType getFileTypeByOffset(PsiFile psiFile, int offset) {
        return PsiUtilBase.getPsiFileAtOffset(psiFile, offset).getFileType();
    }

    private static void removeLineMarkers(@NotNull EditorEx editor) {
        ApplicationManager.getApplication().assertIsDispatchThread();
        RangeHighlighter marker = editor.getUserData(LINE_MARKER_IN_EDITOR_KEY);
        if (marker != null && editor.getMarkupModel().containsHighlighter(marker)) {
            marker.dispose();
        }
        editor.putUserData(LINE_MARKER_IN_EDITOR_KEY, null);
    }

    private void onCaretUpdate(Editor editor, Project project) {
        alarm.cancelAllRequests();
        SelectionModel selectionModel = editor.getSelectionModel();
        if (editor.getProject() != project || selectionModel.hasSelection()) {
            return;
        }
        if (isNotCLFile(editor)) {
            return;
        }
        updateHighlighted(project, editor);
    }

    @Nullable
    private static BraceMatchingUtil.BraceHighlightingAndNavigationContext computeHighlightingAndNavigationContext(@NotNull Editor editor,
                                                                                                                  @NotNull PsiFile file,
                                                                                                                  int offset) {
        EditorHighlighter highlighter = BraceHighlightingHandler.getLazyParsableHighlighterIfAny(file.getProject(), editor, file);
        CharSequence text = editor.getDocument().getCharsSequence();

        HighlighterIterator iterator = highlighter.createIterator(offset);
        FileType fileType = iterator.atEnd() ? null : getFileType(file, iterator.getStart());

        boolean isBeforeOrInsideLeftBrace = fileType != null && isLBraceToken(iterator, text, fileType);
        boolean isBeforeOrInsideRightBrace = !isBeforeOrInsideLeftBrace && fileType != null && isRBraceToken(iterator, text, fileType);
        boolean isInsideBrace = (isBeforeOrInsideLeftBrace || isBeforeOrInsideRightBrace) && iterator.getStart() < offset;

        HighlighterIterator preOffsetIterator = offset > 0 && !isInsideBrace ? highlighter.createIterator(offset - 1) : null;
        FileType preOffsetFileType = preOffsetIterator != null ? getFileType(file, preOffsetIterator.getStart()) : null;

        boolean isAfterLeftBrace = preOffsetIterator != null &&
                isLBraceToken(preOffsetIterator, text, preOffsetFileType);
        boolean isAfterRightBrace = !isAfterLeftBrace && preOffsetIterator != null &&
                isRBraceToken(preOffsetIterator, text, preOffsetFileType);

        int offsetTokenStart = iterator.atEnd() ? -1 : iterator.getStart();
        int preOffsetTokenStart = preOffsetIterator == null || preOffsetIterator.atEnd() ? -1 : preOffsetIterator.getStart();

        if (editor.getSettings().isBlockCursor()) {
            if (isBeforeOrInsideLeftBrace && matchBrace(text, fileType, iterator, true)) {
                return new BraceHighlightingAndNavigationContext(offsetTokenStart, iterator.getStart(), isInsideBrace);
            } else if (isBeforeOrInsideRightBrace && matchBrace(text, fileType, iterator, false)) {
                return new BraceHighlightingAndNavigationContext(offsetTokenStart, iterator.getStart(), isInsideBrace);
            } else if (isAfterRightBrace && matchBrace(text, preOffsetFileType, preOffsetIterator, false)) {
                return new BraceHighlightingAndNavigationContext(preOffsetTokenStart, preOffsetIterator.getStart(), true);
            } else if (isAfterLeftBrace && matchBrace(text, preOffsetFileType, preOffsetIterator, true)) {
                return new BraceHighlightingAndNavigationContext(preOffsetTokenStart, preOffsetIterator.getStart(), true);
            }
        } else {
            if (isAfterRightBrace && matchBrace(text, preOffsetFileType, preOffsetIterator, false)) {
                return new BraceHighlightingAndNavigationContext(preOffsetTokenStart, preOffsetIterator.getStart(), true);
            } else if (isBeforeOrInsideLeftBrace && matchBrace(text, fileType, iterator, true)) {
                return new BraceHighlightingAndNavigationContext(offsetTokenStart, iterator.getEnd(), isInsideBrace);
            } else if (isAfterLeftBrace && matchBrace(text, preOffsetFileType, preOffsetIterator, true)) {
                return new BraceHighlightingAndNavigationContext(preOffsetTokenStart, preOffsetIterator.getEnd(), true);
            } else if (isBeforeOrInsideRightBrace && matchBrace(text, fileType, iterator, false)) {
                return new BraceHighlightingAndNavigationContext(offsetTokenStart, iterator.getStart(), isInsideBrace);
            }
        }
        return null;
    }

    private static boolean isLBraceToken(@NotNull HighlighterIterator iterator, @NotNull CharSequence fileText, @NotNull FileType fileType) {
        IElementType tokenType = iterator.getTokenType();
        return tokenType.equals(LispTypes.LPAREN);
    }

    private static boolean isRBraceToken(@NotNull HighlighterIterator iterator, @NotNull CharSequence fileText, @NotNull FileType fileType) {
        IElementType tokenType = iterator.getTokenType();
        return tokenType.equals(LispTypes.RPAREN);
    }

    @NotNull
    private static FileType getFileType(PsiFile file, int offset) {
        return PsiUtilBase.getPsiFileAtOffset(file, offset).getFileType();
    }

    private static class MatchBraceContext {
        private final CharSequence fileText;
        private final FileType fileType;
        private final HighlighterIterator iterator;
        private final boolean forward;

        private final IElementType brace1Token;
        private final int group;

        private final Stack<IElementType> myBraceStack = new Stack<>();

        MatchBraceContext(@NotNull CharSequence fileText,
                          @NotNull FileType fileType,
                          @NotNull HighlighterIterator iterator,
                          boolean forward) {
            this.fileText = fileText;
            this.fileType = fileType;
            this.iterator = iterator;
            this.forward = forward;

            brace1Token = this.iterator.getTokenType();
            group = getTokenGroup(brace1Token, this.fileType);
        }

        private boolean doBraceMatch() {
            myBraceStack.clear();
            myBraceStack.push(brace1Token);
            boolean matched = false;
            while (true) {
                advance(iterator, forward);
                if (iterator.atEnd()) {
                    break;
                }

                IElementType tokenType = iterator.getTokenType();

                if (getTokenGroup(tokenType, fileType) != group) {
                    continue;
                }

                if (isBraceToken(iterator, fileText, fileType, !forward)) {
                    myBraceStack.push(tokenType);
                } else if (isBraceToken(iterator, fileText, fileType, forward)) {
                    IElementType topTokenType = myBraceStack.pop();

                    IElementType baseType = getOppositeBraceTokenType(tokenType);
                    if (myBraceStack.contains(baseType)) {
                        while (!isPairBraces(topTokenType, tokenType, fileType) && !myBraceStack.empty()) {
                            topTokenType = myBraceStack.pop();
                        }
                    }

                    if (!isPairBraces(topTokenType, tokenType, fileType)) {
                        break;
                    }

                    if (myBraceStack.isEmpty()) {
                        matched = true;
                        break;
                    }
                }
            }
            return matched;
        }

        private IElementType getOppositeBraceTokenType(IElementType tokenType) {
            if (tokenType.equals(LispTypes.LPAREN))
                return LispTypes.RPAREN;
            else if (tokenType.equals(LispTypes.RPAREN))
                return LispTypes.LPAREN;
            return null;
        }
    }

    private static synchronized boolean matchBrace(@NotNull CharSequence fileText,
                                                   @NotNull FileType fileType,
                                                   @NotNull HighlighterIterator iterator,
                                                   boolean forward) {
        return new MatchBraceContext(fileText, fileType, iterator, forward).doBraceMatch();
    }


    private static boolean isPairBraces(@NotNull IElementType tokenType1, @NotNull IElementType tokenType2, @NotNull FileType fileType) {
        return (tokenType1.equals(LispTypes.LPAREN) && tokenType2.equals(LispTypes.RPAREN)) || (tokenType1.equals(LispTypes.RPAREN) && tokenType2.equals(LispTypes.LPAREN));
    }

    private static int getTokenGroup(@Nullable IElementType tokenType, FileType fileType) {
        return LispTypes.LPAREN.equals(tokenType) || LispTypes.RPAREN.equals(tokenType) ? SltCommonLispLanguage.INSTANCE.hashCode() : -1;
    }

    private static boolean isBraceToken(@NotNull HighlighterIterator iterator,
                                        @NotNull CharSequence fileText,
                                        @NotNull FileType fileType,
                                        boolean searchingForRight) {
        return searchingForRight ? isRBraceToken(iterator, fileText, fileType)
                : isLBraceToken(iterator, fileText, fileType);
    }

    private static void advance(@NotNull HighlighterIterator iterator, boolean forward) {
        if (forward) {
            iterator.advance();
        } else {
            iterator.retreat();
        }
    }
}
