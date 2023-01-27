import com.en_circle.slt.plugin.SltCommonLispFileType;
import com.en_circle.slt.plugin.SltCommonLispLanguage;
import com.en_circle.slt.plugin.SltCommonLispParserDefinition;
import com.en_circle.slt.plugin.lisp.psi.LispCoreProjectEnvironment;
import com.en_circle.slt.plugin.lisp.psi.LispVisitor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import org.jetbrains.annotations.NotNull;

public class ParserTest {

    public static void main(String[] args) {
        String data = """
                (:debug 6 1 ("There is no class named COMMON-LISP-USER::COP." "   [Condition of type SB-PCL:CLASS-NOT-FOUND-ERROR]" nil) (("ABORT" "Abort compilation." nil) ("*ABORT" "Return to SLIME's top level." (common-lisp:&rest sb-kernel::temp)) ("ABORT" "abort thread (#<THREAD \\"worker\\" RUNNING {10021FF8E3}>)" nil)) ((0 "(SB-PCL::FIND-CLASS-FROM-CELL COP #<SB-KERNEL::CLASSOID-CELL COP> T)" sb-pcl::find-class-from-cell (:location (:file "/usr/share/sbcl-source/src/pcl/macros.lisp") (:position 3107) (:snippet "(error 'class-not-found-error :name symbol))))

                (defun find-class (symbol &optional (errorp t) environment)
                  (declare (ignore environment) (explicit-check))
                  (find-class-from-cell symbol
                                        (find-classoid-cell symbol)
                               ")) "SB-PCL") (1 "((SB-C::TOP-LEVEL-FORM (SB-PCL::LOAD-DEFMETHOD (QUOTE STANDARD-METHOD) (QUOTE XANTHYPA) (QUOTE NIL) (LIST (FIND-CLASS (QUOTE COP))) (QUOTE (ROBOCOP)) (LIST* :FUNCTION (LET* (#1=# #1#) (SETF #1# #1#) S.." (sb-c::top-level-form (sb-pcl::load-defmethod (quote common-lisp:standard-method) (quote common-lisp-user::xanthypa) (quote nil) (common-lisp:list (common-lisp:find-class (quote common-lisp-user::cop))) (quote (common-lisp-user::robocop)) (common-lisp:list* :function (common-lisp:let* (# #) (common-lisp:setf # #) sb-pcl::mf) (quote (sb-pcl::plist # sb-pcl::simple-next-method-call t))) (sb-c:source-location))) (:location (:buffer "/home/pvan/test/asd.cl") (:offset 209 0) (:snippet "(defmethod xanthypa ((robocop cop))
                  (print cop))")) nil) (2 "(SB-FASL::LOAD-FASL-GROUP #S(SB-FASL::FASL-INPUT :STREAM #<SB-SYS:FD-STREAM for \\"file /tmp/slimecgXcrt.fasl\\" {10022A37B3}> :TABLE #(115 #1=\\"/home/pvan/test/asd.cl\\" :EMACS-BUFFER #2=\\"/home/pvan/test/as.." sb-fasl::load-fasl-group (:location (:file "/usr/share/sbcl-source/src/code/load.lisp") (:position 30966) (:snippet "(funcall function fasl-input arg1))
                                           (2 (funcall function fasl-input arg1 arg2))
                                           (3 (funcall function fasl-input arg1 arg2 arg3)))
                                         (error \\"corrupt fasl file: FOP code #x~x\\" byte)")) "SB-FASL") (3 "((LAMBDA NIL :IN SB-FASL::LOAD-AS-FASL))" (common-lisp:lambda nil :in sb-fasl::load-as-fasl) (:location (:file "/usr/share/sbcl-source/src/code/load.lisp") (:position 32351) (:snippet "(load-fasl-group fasl-input))
                        ;; Nuke the table and stack to avoid keeping garbage on
                        ;; conservatively collected platforms.
                        (nuke-fop-vector (%fasl-input-table fasl-input))
                        (nuke-fop-vector (%fasl-input-stack fasl-input))))")) nil) (4 "(SB-IMPL::CALL-WITH-LOADER-PACKAGE-NAMES #<FUNCTION (LAMBDA NIL :IN SB-FASL::LOAD-AS-FASL) {10022A630B}>)" sb-impl::call-with-loader-package-names (:location (:file "/usr/share/sbcl-source/src/code/target-package.lisp") (:position 103223) (:snippet "(funcall function)
                    (when (and (not boundp) *deferred-package-names*)
                      (info-maphash
                       (lambda (name package)
                         (unless (eq package :deleted)
                           (dovector (sym (symtbl-cells (package-internal-symbols package)))
                             (whe")) "SB-IMPL") (5 "(SB-FASL::LOAD-AS-FASL #<SB-SYS:FD-STREAM for \\"file /tmp/slimecgXcrt.fasl\\" {10022A37B3}> NIL NIL)" sb-fasl::load-as-fasl (:location (:file "/usr/share/sbcl-source/src/code/load.lisp") (:position 32279) (:snippet "(with-loader-package-names
                      (unwind-protect
                           (loop while (load-fasl-group fasl-input))
                        ;; Nuke the table and stack to avoid keeping garbage on
                        ;; conservatively collected platforms.
                        (nuke-fop-vector (%fasl-input-table")) "SB-FASL") (6 "((LABELS SB-FASL::LOAD-STREAM-1 :IN LOAD) #<SB-SYS:FD-STREAM for \\"file /tmp/slimecgXcrt.fasl\\" {10022A37B3}> T)" (common-lisp:labels sb-fasl::load-stream-1 :in common-lisp:load) (:location (:file "/usr/share/sbcl-source/src/code/target-load.lisp") (:position 12454) (:snippet "(load-as-fasl stream verbose (if print t nil))
                                   ;; FIXME: if *EVALUATOR-MODE* is :INTERPRET,
                                   ;; then this should have nothing whatsoever to do with
                                   ;; compiler-error-resignaling. That's an artifact
                  ")) nil) (7 "(SB-FASL::CALL-WITH-LOAD-BINDINGS #<FUNCTION (LABELS SB-FASL::LOAD-STREAM-1 :IN LOAD) {7FAD51A6DFEB}> #<SB-SYS:FD-STREAM for \\"file /tmp/slimecgXcrt.fasl\\" {10022A37B3}> T #<SB-SYS:FD-STREAM for \\"file /.." sb-fasl::call-with-load-bindings (:location (:file "/usr/share/sbcl-source/src/code/target-load.lisp") (:position 6006) (:snippet "(defun call-with-load-bindings (function stream arg pathname-designator)
                  (let* (;; FIXME: we should probably document the circumstances
                         ;; where *LOAD-PATHNAME* and *LOAD-TRUENAME* aren't
                         ;; pathnames during LOAD.  ANSI makes no excepti")) "SB-FASL") (8 "(LOAD #P\\"/tmp/slimecgXcrt.fasl\\" :VERBOSE NIL :PRINT NIL :IF-DOES-NOT-EXIST :ERROR :EXTERNAL-FORMAT :DEFAULT)" common-lisp:load (:location (:file "/usr/share/sbcl-source/src/code/target-load.lisp") (:position 9728) (:snippet "(defun load (filespec &key (verbose *load-verbose*) (print *load-print*)
                                           (if-does-not-exist :error) (external-format :default))
                  \\"Load the file given by FILESPEC into the Lisp environment, returning T on
                   success. The file typ")) "COMMON-LISP") (9 "((FLET SWANK/BACKEND:CALL-WITH-COMPILATION-HOOKS :IN \\"/usr/share/common-lisp/source/swank/swank/sbcl.lisp\\") #<FUNCTION (LAMBDA NIL :IN SWANK/BACKEND:SWANK-COMPILE-STRING) {10022A369B}>)" (common-lisp:flet swank/backend:call-with-compilation-hooks :in "/usr/share/common-lisp/source/swank/swank/sbcl.lisp") (:location (:file "/usr/share/common-lisp/source/swank/swank/sbcl.lisp") (:position 25939) (:snippet "(funcall function)))

                ;;; HACK: SBCL 1.2.12 shipped with a bug where
                ;;; SB-EXT:RESTRICT-COMPILER-POLICY would signal an error when there
                ;;; were no policy restrictions in place. This workaround ensures the
                ;;; existence of at least one dummy restriction.")) nil) (10 "((FLET SWANK/BACKEND:SWANK-COMPILE-STRING :IN \\"/usr/share/common-lisp/source/swank/swank/sbcl.lisp\\") \\"(defmethod xanthypa ((robocop cop)) ..)" (common-lisp:flet swank/backend:swank-compile-string :in "/usr/share/common-lisp/source/swank/swank/sbcl.lisp") (:location (:file "/usr/share/common-lisp/source/swank/swank/sbcl.lisp") (:position 30161) (:snippet "(load-it output-file))
                             (not failurep))
                        (ignore-errors
                          (delete-file *buffer-tmpfile*)
                          (delete-file (compile-file-pathname *buffer-tmpfile*)))))))

                ;;;; Definitions

                (defparameter *definition-types*
                  '(:variable defv")) nil) (11 "((LAMBDA NIL :IN SWANK:COMPILE-STRING-REGION-SLT))" (common-lisp:lambda nil :in swank:compile-string-region-slt) (:location (:file "/src/self/slt/build/idea-sandbox/.slt/SLTinit137/slt.cl") (:position 2993) (:snippet "(swank-compile-string string
                                                 :buffer buffer
                                                 :position offset
                                                 :filename filename))))))

                (export 'slt-eval)
                (export 'compile-string-region-slt)

                (in-package sw")) nil) (12 "((LAMBDA NIL :IN SWANK::COLLECT-NOTES))" (common-lisp:lambda nil :in swank::collect-notes) (:location (:file "/usr/share/common-lisp/source/swank/swank.lisp") (:position 92963) (:snippet "(funcall function))
                               (abort () :report \\"Abort compilation.\\" (list nil))))))
                      (destructuring-bind (successp &optional loadp faslfile) result
                        (let ((faslfile (etypecase faslfile
                                          (null nil)
                                ")) nil) (13 "(SWANK::MEASURE-TIME-INTERVAL #<FUNCTION (LAMBDA NIL :IN SWANK::COLLECT-NOTES) {100223800B}>)" swank::measure-time-interval (:location (:file "/usr/share/common-lisp/source/swank/swank.lisp") (:position 91985) (:snippet "(funcall fun)
                     (/ (- (get-internal-real-time) before)
                        (coerce internal-time-units-per-second 'float)))))

                (defun make-compiler-note (condition)
                  \\"Make a compiler note data structure from a compiler-condition.\\"
                  (declare (type compiler-conditi")) "SWANK") (14 "(SWANK::COLLECT-NOTES #<FUNCTION (LAMBDA NIL :IN SWANK:COMPILE-STRING-REGION-SLT) {10021FFFCB}>)" swank::collect-notes (:location (:file "/usr/share/common-lisp/source/swank/swank.lisp") (:position 92735) (:snippet "(measure-time-interval
                           (lambda ()
                             ;; To report location of error-signaling toplevel forms
                             ;; for errors in EVAL-WHEN or during macroexpansion.
                             (restart-case (multiple-value-list (funcall function))
                        ")) "SWANK") (15 "(SWANK::CALL-WITH-BUFFER-SYNTAX NIL #<FUNCTION (LAMBDA NIL :IN SWANK:COMPILE-STRING-REGION-SLT) {10021FFF8B}>)" swank::call-with-buffer-syntax (:location (:file "/usr/share/common-lisp/source/swank/swank.lisp") (:position 57735) (:snippet "(call-with-syntax-hooks fun)
                        (let ((*readtable* *buffer-readtable*))
                          (call-with-syntax-hooks fun)))))

                (defmacro without-printing-errors ((&key object stream
                                                        (msg \\"<<error printing object>>\\"))
                     ")) "SWANK") (16 "(SB-INT:SIMPLE-EVAL-IN-LEXENV (SWANK:COMPILE-STRING-REGION-SLT \\"(defmethod xanthypa ((robocop cop)) ..)" sb-int:simple-eval-in-lexenv (:location (:file "/usr/share/sbcl-source/src/code/eval.lisp") (:position 13270) (:snippet "(apply (symbol-function name) (args)))
                                    (%simple-eval exp lexenv))))))
                          (t
                           exp))))))
                ) ; end PROGN

                ;;; This definition will be replaced after the interpreter is compiled.
                ;;; Until then we just always compile.
                #+sb-f")) "SB-INT") (17 "(EVAL (SWANK:COMPILE-STRING-REGION-SLT \\"(defmethod xanthypa ((robocop cop)) ..)" common-lisp:eval (:location (:file "/usr/share/sbcl-source/src/code/eval.lisp") (:position 14411) (:snippet "(eval-in-lexenv original-exp nil)))

                (defun eval-tlf (original-exp tlf-index &optional lexenv)
                  (let ((*eval-source-context* original-exp)
                        (*eval-tlf-index* tlf-index)
                        (*eval-source-info* sb-c::*source-info*))
                    (eval-in-lexenv original-")) "COMMON-LISP") (18 "(SWANK:EVAL-FOR-EMACS (SWANK:COMPILE-STRING-REGION-SLT \\"(defmethod xanthypa ((robocop cop)) ..)" swank:eval-for-emacs (:location (:file "/usr/share/common-lisp/source/swank/swank.lisp") (:position 65853) (:snippet "(eval form))))
                           (run-hook *pre-reply-hook*)
                           (setq ok t))
                      (send-to-emacs `(:return ,(current-thread)
                                               ,(if ok
                                                    `(:ok ,result)
                                                    `(:")) "SWANK") (19 "((LAMBDA NIL :IN SWANK::SPAWN-WORKER-THREAD))" (common-lisp:lambda nil :in swank::spawn-worker-thread) (:location (:file "/usr/share/common-lisp/source/swank/swank.lisp") (:position 37885) (:snippet "(apply #'eval-for-emacs\s
                                      (cdr (wait-for-event `(:emacs-rex . _)))))))
                         :name \\"worker\\"))

                (defun add-active-thread (connection thread)
                  (etypecase connection
                    (multithreaded-connection\s
                     (push thread (mconn.active-threa")) nil)) (16))
                """;

        LispCoreProjectEnvironment projectEnvironment = new LispCoreProjectEnvironment();
        projectEnvironment.getEnvironment()
                .registerParserDefinition(SltCommonLispLanguage.INSTANCE, new SltCommonLispParserDefinition());
        PsiFileFactory factory = PsiFileFactory.getInstance(projectEnvironment.getProject());
        PsiFile source = factory.createFileFromText("swank-reply.cl", SltCommonLispFileType.INSTANCE, data);
        source.accept(new LispVisitor() {
            @Override
            public void visitElement(@NotNull PsiElement element) {
                System.out.println(element);
                element.acceptChildren(this);
            }
        });
    }
}
