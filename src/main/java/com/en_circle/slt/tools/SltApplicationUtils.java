package com.en_circle.slt.tools;

import com.en_circle.slt.plugin.SltLispEnvironmentProvider;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationUtil;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.function.Function;

public class SltApplicationUtils {
    private static final Logger log = LoggerFactory.getLogger(SltApplicationUtils.class);

    public static <X> X getAsyncResultNoThrow(Function<Consumer<X>, SlimeRequest> request) {
        return getAsyncResultNoThrow(request, true);
    }

    public static <X> X getAsyncResultNoThrow(Function<Consumer<X>, SlimeRequest> request, boolean startLisp) {
        try {
            return getAsyncResult(request, startLisp);
        } catch (Exception e) {
            log.warn(e.getMessage());
            return null;
        }
    }

    public static <X> X getAsyncResult(Function<Consumer<X>, SlimeRequest> request) throws Exception {
        return getAsyncResult(request, true);
    }

    public static <X> X getAsyncResult(Function<Consumer<X>, SlimeRequest> request, boolean startLisp) throws Exception {
        if (SltLispEnvironmentProvider.getInstance().isLispEnvironmentActive() && !startLisp) {
            return null;
        }

        Future<X> future = ApplicationManager.getApplication().executeOnPooledThread(() -> {
            CyclicBarrier barrier = new CyclicBarrier(2);
            List<X> pointer = new ArrayList<>();
            SltLispEnvironmentProvider.getInstance().sendToLisp(request.apply(result -> {
                pointer.add(result);
                try {
                    barrier.await();
                } catch (Exception ignored) {

                }
            }), startLisp);
            barrier.await();
            return pointer.isEmpty() ? null : pointer.get(0);
        });
        return ApplicationUtil.runWithCheckCanceled(future,
                ProgressIndicatorProvider.getInstance().getProgressIndicator());
    }

}
