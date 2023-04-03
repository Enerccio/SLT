package com.en_circle.slt.plugin.services;

import com.en_circle.slt.plugin.SltBundle;
import com.en_circle.slt.plugin.ui.SltHyperspecView;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.apache.commons.io.IOUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

public class SltProjectService implements DumbAware, Disposable {
    private static final Logger log = LoggerFactory.getLogger(SltProjectService.class);

    public static SltProjectService getInstance(Project project) {
        return project.getService(SltProjectService.class);
    }

    private SltHyperspecView hyperspecView;
    private Map<String, String> symbolRefMap = null;
    private final Project project;

    public SltProjectService(Project project) {
        this.project = project;
        ApplicationManager.getApplication().executeOnPooledThread(this::loadHyperspec);
    }

    private void loadHyperspec() {
        try {
            Map<String, String> symbolParseMap = new HashMap<>();

            String base = "http://www.lispworks.com/documentation/HyperSpec/Front/";
            String html = IOUtils.toString(new URL("http://www.lispworks.com/documentation/HyperSpec/Front/X_AllSym.htm"),
                    Charset.defaultCharset());
            Document document = Jsoup.parse(html);
            Elements links = document.select("a[href]");
            for (Element link : links) {
                if ("DEFINITION".equals(link.attr("REL"))) {
                    String ref = link.text();
                    String page = base + link.attr("HREF");
                    symbolParseMap.put(ref, page);
                }
            }

            symbolRefMap = symbolParseMap;
        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    public void showCLHSSymbol(String symbolName) {
        if (symbolRefMap != null) {
            String url = symbolRefMap.get(symbolName);
            if (url != null) {
                ToolWindow toolWindow = ToolWindowManager.getInstance(project)
                        .getToolWindow("CLHS");
                assert toolWindow != null;
                toolWindow.show(() -> {
                    if (hyperspecView != null) {
                        hyperspecView.showUrl(url);
                    }
                });
            } else {
                Messages.showInfoMessage(String.format(SltBundle.message("slt.ui.clhs.nosymbol.message"), symbolName),
                        SltBundle.message("slt.ui.clhs.nosymbol.title"));
            }
        } else {
            Messages.showInfoMessage(String.format(SltBundle.message("slt.ui.clhs.loading.message"), symbolName),
                    SltBundle.message("slt.ui.clhs.loading.title"));
        }
    }

    public void setHyperspecView(SltHyperspecView hyperspecView) {
        this.hyperspecView = hyperspecView;
    }

    @Override
    public void dispose() {

    }
}
