package com.en_circle.slt.plugin.view;

import com.en_circle.slt.plugin.swank.SwankServer.SwankServerOutput;
import com.en_circle.slt.plugin.swank.SwankStreamController.SwankStreamControllerUpdateListener;

import javax.swing.*;

public interface SltComponent {

    JComponent create();

    void onPreStart();
    void onPostStart();

    void handleOutput(SwankServerOutput output, String data);

    void onPreStop();
    void onPostStop();

    String getTitle();
}
