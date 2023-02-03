package com.en_circle.slt.tools;

import java.util.LinkedList;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class BufferedString {

    private final Consumer<String> simpleAdd;
    private final Consumer<String> completeReset;
    private final LinkedList<String> chunks = new LinkedList<>();
    private int size = 0;
    private int maxSize = 1000000;

    public BufferedString(Consumer<String> simpleAdd, Consumer<String> completeReset) {
        this.simpleAdd = simpleAdd;
        this.completeReset = completeReset;
    }

    public synchronized void append(String data) {
        int len = data.length();
        chunks.add(data);
        size += len;
        if (size > maxSize) {
            truncateAndSet();
        } else {
            simpleAdd.accept(data);
        }
    }

    private void truncateAndSet() {
        int diff = size - maxSize;
        while (!chunks.isEmpty()) {
            String left = chunks.removeFirst();
            if (left != null) {
                if (left.length() < diff) {
                    diff -= left.length();
                    size -= left.length();
                } else {
                    left = left.substring(0, left.length() - diff);
                    size -= diff;
                    chunks.addFirst(left);
                    break;
                }
            }
        }
        completeReset.accept(toString());
    }

    public synchronized void clear() {
        clearNoCall();
        completeReset.accept("");
    }

    public synchronized void clearNoCall() {
        chunks.clear();
        size = 0;
    }

    @Override
    public synchronized String toString() {
        return chunks.stream().filter(Objects::nonNull).collect(Collectors.joining(""));
    }

    public synchronized int getMaxSize() {
        return maxSize;
    }

    public synchronized void setMaxSize(int maxSize) {
        this.maxSize = maxSize;
    }
}
