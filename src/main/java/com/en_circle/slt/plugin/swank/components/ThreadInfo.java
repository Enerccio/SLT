package com.en_circle.slt.plugin.swank.components;

import com.en_circle.slt.plugin.lisp.lisp.*;

import java.util.*;

public class ThreadInfo {

    private int maxHeaders;
    private final Map<String, Integer> headers = new LinkedHashMap<>();
    private final List<Object[]> data = new ArrayList<>();

    public ThreadInfo(LispContainer info) {
        if (info.getItems().size() > 0) {
            parseHeaders(info.getItems().get(0));
        }
        if (info.getItems().size() > 1) {
            for (int ix = 1; ix < info.getItems().size(); ix++) {
                addThreadRown(info.getItems().get(ix));
            }
        }
    }

    private void addThreadRown(LispElement element) {
        if (element instanceof LispContainer c) {
            Object[] rowData = new Object[maxHeaders];
            int ix = 0;
            for (LispElement e : c.getItems()) {
                if (headers.containsValue(ix)) {
                    if (e instanceof LispString str) {
                        rowData[ix] = str.getValue();
                    } else if (e instanceof LispInteger integer) {
                        rowData[ix] = integer.getValue();
                    } else if (e instanceof LispAtom){
                        rowData[ix] = e.toString();
                    }
                }
                ++ix;
            }
            data.add(rowData);
        }
    }

    private void parseHeaders(LispElement element) {
        if (element instanceof LispContainer c) {
            int ix = 0;
            for (LispElement e : c.getItems()) {
                if (e instanceof LispSymbol s) {
                    headers.put(s.getValue().toUpperCase(), ix);
                }
                ++ix;
            }
            maxHeaders = ix;
        }
    }

    public int getMaxHeaders() {
        return maxHeaders;
    }

    public int getRowCount() {
        return data.size();
    }

    public Collection<String> getHeaders() {
        return headers.keySet();
    }

    public String getHeader(int row) {
        return new ArrayList<>(getHeaders()).get(row);
    }

    public String getDataString(int row, int cell) {
        Object data = getData(row, cell);
        if (data == null) {
            return "";
        }
        return Objects.toString(data);
    }

    public Object getData(int row, int cell) {
        if (row < data.size()) {
            if (cell < maxHeaders) {
                return data.get(row)[cell];
            }
        }
        return null;
    }


}
