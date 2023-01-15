package com.en_circle.slt.plugin.environment;

public class SltProcessException extends Exception {

    public SltProcessException() {
    }

    public SltProcessException(String message) {
        super(message);
    }

    public SltProcessException(String message, Throwable cause) {
        super(message, cause);
    }

    public SltProcessException(Throwable cause) {
        super(cause);
    }

    public SltProcessException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
