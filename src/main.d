import dlangutil.testing;

enum Sigil { DATA, SCALAR, ARRAY, NEXT }


unittest {
    run_test({
        assert(false);
    });
    run_test({
        assert(true);
    });
}