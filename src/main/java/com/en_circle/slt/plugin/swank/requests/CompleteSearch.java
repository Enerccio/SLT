package com.en_circle.slt.plugin.swank.requests;

import com.en_circle.slt.plugin.lisp.lisp.LispContainer;
import com.en_circle.slt.plugin.lisp.lisp.LispElement;
import com.en_circle.slt.plugin.lisp.lisp.LispSymbol;
import com.en_circle.slt.plugin.swank.SlimeRequest;
import com.en_circle.slt.plugin.swank.SwankPacket;

import java.math.BigInteger;

public class CompleteSearch extends SlimeRequest {

    public static SlimeRequest search(String prefix, Callback callback) {
        return new CompleteSearch(prefix, null, callback);
    }

    public static SlimeRequest search(String prefix, SearchFilter filter, Callback callback) {
        return new CompleteSearch(prefix, filter, callback);
    }

    private final String prefix;
    private final SearchFilter searchFilter;
    private final Callback callback;

    private CompleteSearch(String prefix, SearchFilter searchFilter, Callback callback) {
        this.prefix = prefix;
        this.searchFilter = searchFilter;
        this.callback = callback;
    }

    @Override
    public SwankPacket createPacket(BigInteger requestId) {
        return SwankPacket.completeSearch(prefix, searchFilter == null ? "NIL" : searchFilter.str, requestId);
    }

    public void processReply(LispContainer data) {
        if (isOk(data)) {
            callback.onResult(data.getItems().get(1));
        }
    }

    private boolean isOk(LispContainer data) {
        return data.getItems().size() > 0 &&
                data.getItems().get(0) instanceof LispSymbol &&
                ":ok".equals(((LispSymbol) data.getItems().get(0)).getValue());
    }

    public interface Callback {
        void onResult(LispElement result);
    }

    public enum SearchFilter {
        CLASS(":CLASS")

        ;

        public final String str;

        SearchFilter(String str) {
            this.str = str;
        }
    }

}
