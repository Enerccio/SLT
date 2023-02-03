package com.en_circle.slt.tests.indentation;

import org.junit.Before;
import org.junit.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class IndentationTests {

    private TestableSltIndentationContainer container;

    @Before
    public void setUpTestContainer() {
        container = new TestableSltIndentationContainer();
        container.init(container.getProject());
    }

    @Test
    public void testInvalid() throws Exception {
        assertEquals(0, container.testIndentCL(""));
        assertEquals(0, container.testIndentCL("4"));
        assertEquals(1, container.testIndentCL(" 4"));
        assertEquals(2, container.testIndentCL("  4"));
    }

    @Test
    public void testFormsBlock() throws Exception {
        assertEquals(4, container.testIndentCL("(block XXX)"));
        assertEquals(2, container.testIndentCL("(block foo XXX)"));
        assertEquals(6, container.testIndentCL("  (block XXX)"));
        assertEquals(4, container.testIndentCL("  (block foo XXX)"));
    }

    @Test
    public void testFormsCase() throws Exception {
        assertEquals(4, container.testIndentCL("(case XXX)"));
        assertEquals(2, container.testIndentCL("(case blah XXX)"));
        assertEquals(12, container.testIndentCL("(case blah (XXX))"));
        assertEquals(18, container.testIndentCL("(case blah (wot) (XXX))"));
        assertEquals(12, container.testIndentCL("(case blah (foo XXX))"));
        assertEquals(3, container.testIndentCL("""
                (case
                    blah
                  (foo XXX))"""));

        assertEquals(4, container.testIndentCL("""
                 (case
                     blah
                   (foo XXX))\
                """));

        assertEquals(4, container.testIndentCL("(ecase XXX)"));
        assertEquals(4, container.testIndentCL("(ccase XXX)"));
        assertEquals(4, container.testIndentCL("(typecase XXX)"));
        assertEquals(4, container.testIndentCL("(etypecase XXX)"));
        assertEquals(4, container.testIndentCL("(ctypecase XXX)"));
    }

    @Test
    public void testFormsCatch() throws Exception {
        assertEquals(4, container.testIndentCL("(catch XXX)"));
        assertEquals(2, container.testIndentCL("(catch foo XXX)"));
        assertEquals(6, container.testIndentCL("  (catch XXX)"));
        assertEquals(4, container.testIndentCL("  (catch foo XXX)"));
    }

    @Test
    public void testFormsCond() throws Exception {
        assertEquals(2, container.testIndentCL("(cond XXX)"));
        assertEquals(8, container.testIndentCL("(cond (XXX))"));
        assertEquals(10, container.testIndentCL("(cond (foo XXX))"));
        assertEquals(7+4, container.testIndentCL("(cond ((= 1 XXX)))"));
        assertEquals(18, container.testIndentCL("(cond (foo bar) (XXX))"));
        assertEquals(20, container.testIndentCL("(cond (foo bar) (baz XXX))"));
    }

    @Test
    public void testFormsConstructor() throws Exception {
        assertEquals(4, container.testIndentCL("(:constructor XXX)"));
        assertEquals(4, container.testIndentCL("(:constructor foo XXX)"));
        assertEquals(6, container.testIndentCL("  (:constructor XXX)"));
        assertEquals(6, container.testIndentCL("  (:constructor foo XXX)"));
    }

}
