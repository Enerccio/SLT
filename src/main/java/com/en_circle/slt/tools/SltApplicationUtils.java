package com.en_circle.slt.tools;

import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService;
import com.en_circle.slt.plugin.services.lisp.LispEnvironmentService.LispEnvironmentState;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationUtil;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import org.awaitility.Awaitility;
import org.awaitility.core.ConditionTimeoutException;
import org.awaitility.pollinterval.FixedPollInterval;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Function;

public class SltApplicationUtils {
    private static final Logger log = LoggerFactory.getLogger(SltApplicationUtils.class);

    public static <X> X getAsyncResultNoThrow(Project project, Function<Consumer<X>, SlimeRequest> request) {
        return getAsyncResultNoThrow(project, request, true);
    }

    public static <X> X getAsyncResultNoThrow(Project project, Function<Consumer<X>, SlimeRequest> request, boolean startLisp) {
        try {
            return getAsyncResult(project, request, startLisp);
        } catch (Exception e) {
            log.warn(e.getMessage());
            return null;
        }
    }

    public static <X> X getAsyncResult(Project project, Function<Consumer<X>, SlimeRequest> request) throws Exception {
        return getAsyncResult(project, request, true);
    }

    public static <X> X getAsyncResult(Project project, Function<Consumer<X>, SlimeRequest> request, boolean startLisp) throws Exception {
        Future<X> future = getAsyncResultNoProgress(project, request, startLisp);
        if (future == null) {
            return null;
        }

        return ApplicationUtil.runWithCheckCanceled(future,
                ProgressIndicatorProvider.getInstance().getProgressIndicator());
    }

    public static <X> Future<X> getAsyncResultNoProgress(Project project, Function<Consumer<X>, SlimeRequest> request, boolean startLisp) {
        if (LispEnvironmentService.getInstance(project).getState() != LispEnvironmentState.READY && !startLisp) {
            return null;
        }

        return ApplicationManager.getApplication().executeOnPooledThread(() -> {
            BlockingQueue<X> pointer = new ArrayBlockingQueue<>(1);
            SlimeRequest r = request.apply(result ->
                    ApplicationManager.getApplication().invokeLater(() ->
                            ApplicationManager.getApplication().runWriteAction(() -> {
                                try {
                                    pointer.put(result);
                                } catch (Exception ignored) {

                                }
                            })));
            LispEnvironmentService.getInstance(project).sendToLisp(r, startLisp);
            try {
                Awaitility.await()
                        .atMost(2, TimeUnit.SECONDS)
                        .pollInterval(new FixedPollInterval(10, TimeUnit.MILLISECONDS))
                        .failFast(ProgressManager::checkCanceled)
                        .until(() -> pointer.peek() != null);
            } catch (ConditionTimeoutException exception) {
                return null;
            }
            return pointer.isEmpty() ? null : pointer.take();
        });
    }

    public static <X> X getAsyncResultCheckCancellation(Project project, Function<Consumer<X>, SlimeRequest> request, boolean startLisp) throws Exception {
        Future<X> future = getAsyncResultNoProgress(project, request, startLisp);
        if (future == null) {
            return null;
        }

        ProgressIndicator pi = ProgressIndicatorProvider.getInstance().getProgressIndicator();

        while (!future.isDone()) {
            Thread.sleep(1);
            if (pi != null) {
                ProgressManager.checkCanceled();
            }
        }
        return future.get();
    }

    public static <T> T processAsync(Computable<T> supplier) {
        try {
            return ApplicationManager.getApplication().executeOnPooledThread(supplier::get).get();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
