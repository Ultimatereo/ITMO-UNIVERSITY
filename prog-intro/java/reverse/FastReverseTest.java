package reverse;

import base.ModelessSelector;
import base.TestCounter;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class FastReverseTest {
    public static final int MAX_SIZE = 1_000_000 / TestCounter.DENOMINATOR;
    public static final ModelessSelector<?> SELECTOR = ReverseTest.selector(FastReverseTest.class, MAX_SIZE);

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
