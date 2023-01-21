package com.en_circle.slt.tools;

public class RoswellUtils {

    public static boolean exists() {
        try {
            Runtime.getRuntime().exec("ros");
            return true;
        } catch (Exception ignored) {

        }
        return false;
    }

}
