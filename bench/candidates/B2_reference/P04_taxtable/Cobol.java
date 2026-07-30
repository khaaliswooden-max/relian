
import java.math.BigDecimal;
import java.math.RoundingMode;

/** COBOL arithmetic semantics. COBOL ROUNDED is HALF_UP (away from zero);
 *  COMPUTE without ROUNDED TRUNCATES toward zero to the receiving PIC scale. */
final class Cobol {
    static BigDecimal rounded(BigDecimal v, int scale) {
        return v.setScale(scale, RoundingMode.HALF_UP);
    }
    static BigDecimal trunc(BigDecimal v, int scale) {
        return v.setScale(scale, RoundingMode.DOWN);
    }
    static String num(String s) {
        s = s.trim();
        return s.isEmpty() ? "0" : s;
    }
    /** PIC Z(n)9.99 / -(n)9.99 then FUNCTION TRIM -> plain decimal, no pad. */
    static String disp(BigDecimal v, int scale) {
        return v.setScale(scale, RoundingMode.DOWN).toPlainString();
    }
    static String[] unstring(String line, int n) {
        String[] out = new String[n];
        String[] p = line.split(",", -1);
        for (int i = 0; i < n; i++) out[i] = i < p.length ? p[i].trim() : "";
        return out;
    }
}
