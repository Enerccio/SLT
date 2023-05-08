package com.en_circle.slt.plugin.actions;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.lisp.lisp.LispString;
import com.en_circle.slt.plugin.lisp.lisp.LispUtils;
import com.en_circle.slt.plugin.lisp.psi.LispList;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.swank.requests.Macroexpand;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MacroexpandAction extends MacroexpandActionBase {
    private static final Logger log = LoggerFactory.getLogger(MacroexpandAction.class);

    @Override
    protected void macroexpand(Project project, LispList form, String packageName, MacroexpandCallback callback) {
        try {
            LispEnvironmentService service = LispEnvironmentService.getInstance(project);
            service.sendToLisp(new Macroexpand(form.getText(), packageName, result -> {
                callback.showMacroexpand(LispUtils.unescape(((LispString) result).getValue()));
            }), true);
        } catch (Exception e) {
            log.warn(SltBundle.message("slt.error.start"), e);
            Messages.showErrorDialog(project, e.getMessage(), SltBundle.message("slt.ui.errors.lisp.start"));
        }
    }
}
