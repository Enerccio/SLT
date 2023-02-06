package com.en_circle.slt.plugin.swank.debug;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class SltInspectedObject {
    private static final Logger log = LoggerFactory.getLogger(SltInspectedObject.class);

    private String title;
    private BigInteger id;
    private final List<SltInspectionElement> elements = new ArrayList<>();

    public SltInspectedObject() {

    }

    public String getTitle() {
        return title;
    }

    public BigInteger getId() {
        return id;
    }

    public List<SltInspectionElement> getElements() {
        return elements;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setId(BigInteger id) {
        this.id = id;
    }

    public static class SltInspectionElement {

        private String text;
        private BigInteger id = null;
        private boolean titled = false;

        public String getText() {
            return text;
        }

        public BigInteger getId() {
            return id;
        }

        public void setText(String text) {
            this.text = text;
        }

        public void setId(BigInteger id) {
            this.id = id;
        }

        public boolean isTitled() {
            return titled;
        }

        public void setTitled(boolean titled) {
            this.titled = titled;
        }
    }
}
