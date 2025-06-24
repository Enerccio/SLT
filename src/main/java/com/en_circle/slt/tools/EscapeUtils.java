package com.en_circle.slt.tools;

import com.google.common.html.HtmlEscapers;

public class EscapeUtils {

    public static String escapeHtml4(String html) {
        return HtmlEscapers.htmlEscaper().escape(html);
    }

}
