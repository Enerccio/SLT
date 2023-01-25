package com.en_circle.slt.plugin.environment;

import org.awaitility.Awaitility;
import org.awaitility.pollinterval.FixedPollInterval;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BooleanSupplier;

public class SltProcessStreamGobbler extends Thread {

    private final InputStream inputStream;
    private final List<SltProcessStreamControllerUpdateListener> updateListeners = Collections.synchronizedList(new ArrayList<>());
    private volatile boolean alive = true;

    public SltProcessStreamGobbler(InputStream inputStream) {
        this.inputStream = inputStream;
        setName("SltProcessStreamGobbler " + inputStream);
        setDaemon(true);
    }

    public void addUpdateListener(SltProcessStreamControllerUpdateListener listener) {
        updateListeners.add(listener);
    }

    public void run() {
        try {
            byte[] buffer = new byte[2048];

            while (true) {
                if (Thread.interrupted())
                    return;

                int readSize = inputStream.read(buffer);

                if (Thread.interrupted())
                    return;
                if (readSize == -1)
                    return;

                if (readSize > 0) {
                    String data = new String(buffer, 0, readSize, StandardCharsets.UTF_8);
                    for (SltProcessStreamControllerUpdateListener listener : updateListeners) {
                        listener.onDataRead(data);
                    }
                }
            }
        } catch (Exception ignored) {
            // pass
        }
        alive = false;
    }

    private boolean isFinished() {
        return !alive;
    }

    public interface SltProcessStreamControllerUpdateListener {

        void onDataRead(String data);

    }

    public interface ProcessInitializationWaiter {
        boolean awaitFor(Process process);

    }

    public static class WaitForOccurrence implements ProcessInitializationWaiter, SltProcessStreamControllerUpdateListener {

        private final String occurrence;
        private final String[] occurrenceStrings;
        private int pos;
        private volatile boolean status = false;

        public WaitForOccurrence(String occurrence) {
            assert(occurrence.length() > 0);

            this.occurrence = occurrence;
            this.occurrenceStrings = new String[occurrence.length()];
            for (int i=0; i<occurrence.length(); i++) {
                this.occurrenceStrings[i] = occurrence.substring(i, i+1);
            }
            this.pos = 0;
        }

        public boolean awaitFor(Process process) {
            return awaitFor(process, null, 10L, TimeUnit.SECONDS);
        }

        public boolean awaitFor(Process process, SltProcessStreamGobbler gobbler, long time, TimeUnit unit) {
            return awaitFor(process, gobbler, time, unit, null);
        }

        public boolean awaitFor(Process process, SltProcessStreamGobbler gobbler, long time, TimeUnit unit, BooleanSupplier checkForEnd) {
            AtomicBoolean started = new AtomicBoolean(false);
            Awaitility.await()
                    .pollInterval(new FixedPollInterval(250, TimeUnit.MILLISECONDS))
                    .atMost(time, unit)
                    .until(() -> {
                        if (checkForEnd != null && checkForEnd.getAsBoolean()) {
                            return true;
                        }
                        if (process != null && !process.isAlive()) {
                            return true;
                        }
                        if (gobbler != null && gobbler.isFinished()) {
                            return true;
                        }
                        if (status) {
                            started.set(true);
                            return true;
                        }
                        return false;
                    });
            return started.get();
        }

        @Override
        public void onDataRead(String data) {
            if (status) {
                return;
            }

            if (pos == 0) {
                if (data.contains(occurrence)) {
                    status = true;
                }
                if (data.endsWith(occurrenceStrings[0])) {
                    pos++;
                }
            } else {
                while (true) {
                    if (pos == occurrence.length()) {
                        status = true;
                        return;
                    }
                    if (data.length() == 0) {
                        return;
                    }

                    if (data.startsWith(occurrenceStrings[pos])) {
                        data = data.substring(1);
                        pos++;
                    } else {
                        pos = 0;
                        return;
                    }
                }
            }
        }
    }

}
