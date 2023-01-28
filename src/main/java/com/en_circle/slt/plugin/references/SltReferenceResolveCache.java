package com.en_circle.slt.plugin.references;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.intellij.psi.ResolveResult;
import org.jetbrains.annotations.NotNull;

import java.time.Duration;
import java.time.temporal.ChronoUnit;

public class SltReferenceResolveCache {

    private final LoadingCache<SltReference, ResolveResult[]> referenceCache;

    public SltReferenceResolveCache() {
        CacheLoader<SltReference, ResolveResult[]> loader = new CacheLoader<>() {
            @Override
            public ResolveResult @NotNull [] load(SltReference key) throws Exception {
                return key.resolveInner(false);
            }
        };

        referenceCache = CacheBuilder.newBuilder()
                .maximumSize(4096)
                .expireAfterAccess(Duration.of(500, ChronoUnit.MILLIS))
                .build(loader);
    }

    public ResolveResult[] resolve(SltReference sltReference) {
        try {
            return referenceCache.get(sltReference);
        } catch (Exception ignored) {
            return new ResolveResult[0];
        }
    }
}
