package com.en_circle.slt.plugin.contributors;

import com.en_circle.slt.plugin.swank.requests.CompleteSearch.SearchFilter;

public class SltClassContributor extends SltSymbolContributor {

    @Override
    protected SearchFilter getFilter() {
        return SearchFilter.CLASS;
    }

}
