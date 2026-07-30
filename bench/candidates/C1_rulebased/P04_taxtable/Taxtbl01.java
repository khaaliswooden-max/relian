
import java.math.BigDecimal;
import java.math.RoundingMode;

final class R {
    /* edited picture then FUNCTION TRIM -> plain decimal at edit scale */
    static String dedit(BigDecimal v, int dec) {
        return v.setScale(dec, RoundingMode.DOWN).toPlainString();
    }
    /* unedited unsigned integer numeric DISPLAY: zero-pad to declared digits */
    static String dnumU(BigDecimal v, int intd) {
        String ip = v.setScale(0, RoundingMode.DOWN).toPlainString();
        while (ip.length() < intd) ip = "0" + ip;
        return ip;
    }
    static boolean eq(String a, String b) { return rtrim(a).equals(rtrim(b)); }
    static String rtrim(String s) {
        int e = s.length();
        while (e > 0 && s.charAt(e-1) == ' ') e--;
        return s.substring(0, e);
    }
    static int ix(BigDecimal v) { return v.intValue(); }
    static int ix(int v) { return v; }
    static String take(String s, int len) {
        return s.substring(0, Math.min(s.length(), len));
    }
    static String[] unstring(String line, int n, String delim) {
        String[] out = new String[n];
        java.util.Arrays.fill(out, "");
        String[] p = line.split(java.util.regex.Pattern.quote(delim), -1);
        System.arraycopy(p, 0, out, 0, Math.min(n, p.length));
        return out;
    }
}

public class Taxtbl01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    String WS_F1 = "";
    String WS_F2 = "";
    BigDecimal WS_INC = BigDecimal.ZERO;
    String WS_ST = "";
    BigDecimal[] BR_CEIL = new BigDecimal[5];
    java.util.Arrays.fill(BR_CEIL, BigDecimal.ZERO);
    BigDecimal[] BR_RATE = new BigDecimal[5];
    java.util.Arrays.fill(BR_RATE, BigDecimal.ZERO);
    BigDecimal[] BR_BASE = new BigDecimal[5];
    java.util.Arrays.fill(BR_BASE, BigDecimal.ZERO);
    BigDecimal WS_TAX = BigDecimal.ZERO;
    BigDecimal WS_IDX = BigDecimal.ZERO;
    BigDecimal WS_PREV = BigDecimal.ZERO;
    BigDecimal WS_MULT = BigDecimal.ZERO;
    String WS_O_TAX = "";
    String WS_O_MARG = "";
    int BI = 1;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 2, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_INC = (new BigDecimal(WS_F1.trim())).setScale(2, RoundingMode.DOWN);
    WS_ST = R.take(WS_F2.trim(), 1);
    if (R.eq(WS_ST, "M")) {
      WS_MULT = (new BigDecimal("2.00")).setScale(2, RoundingMode.DOWN);
    } else {
      WS_MULT = (new BigDecimal("1.00")).setScale(2, RoundingMode.DOWN);
    }
    BR_CEIL[R.ix(new BigDecimal("1"))-1] = (new BigDecimal("11600.00").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_RATE[R.ix(new BigDecimal("1"))-1] = (new BigDecimal("0.1000")).setScale(4, RoundingMode.DOWN);
    BR_BASE[R.ix(new BigDecimal("1"))-1] = (new BigDecimal("0.00")).setScale(2, RoundingMode.DOWN);
    BR_CEIL[R.ix(new BigDecimal("2"))-1] = (new BigDecimal("47150.00").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_RATE[R.ix(new BigDecimal("2"))-1] = (new BigDecimal("0.1200")).setScale(4, RoundingMode.DOWN);
    BR_BASE[R.ix(new BigDecimal("2"))-1] = (new BigDecimal("1160.00").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_CEIL[R.ix(new BigDecimal("3"))-1] = (new BigDecimal("100525.00").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_RATE[R.ix(new BigDecimal("3"))-1] = (new BigDecimal("0.2200")).setScale(4, RoundingMode.DOWN);
    BR_BASE[R.ix(new BigDecimal("3"))-1] = (new BigDecimal("5426.00").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_CEIL[R.ix(new BigDecimal("4"))-1] = (new BigDecimal("191950.00").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_RATE[R.ix(new BigDecimal("4"))-1] = (new BigDecimal("0.2400")).setScale(4, RoundingMode.DOWN);
    BR_BASE[R.ix(new BigDecimal("4"))-1] = (new BigDecimal("17168.50").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BR_CEIL[R.ix(new BigDecimal("5"))-1] = (new BigDecimal("999999999.00")).setScale(2, RoundingMode.DOWN);
    BR_RATE[R.ix(new BigDecimal("5"))-1] = (new BigDecimal("0.3200")).setScale(4, RoundingMode.DOWN);
    BR_BASE[R.ix(new BigDecimal("5"))-1] = (new BigDecimal("39110.50").multiply(WS_MULT)).setScale(2, RoundingMode.DOWN);
    BI = 1;
    boolean _found = false;
    for (; BI <= 5; BI++) {
      if ((WS_INC).compareTo(BR_CEIL[R.ix(BI)-1]) <= 0) {
        WS_IDX = (new BigDecimal(BI)).setScale(0, RoundingMode.DOWN);
        _found = true; break;
      }
    }
    if (!_found) {
      WS_IDX = (new BigDecimal("5")).setScale(0, RoundingMode.DOWN);
    }
    if ((WS_IDX).compareTo(new BigDecimal("1")) == 0) {
      WS_PREV = (BigDecimal.ZERO).setScale(2, RoundingMode.DOWN);
    } else {
      WS_PREV = (BR_CEIL[R.ix(WS_IDX.subtract(new BigDecimal("1")))-1]).setScale(2, RoundingMode.DOWN);
    }
    WS_TAX = (BR_BASE[R.ix(WS_IDX)-1].add(WS_INC.subtract(WS_PREV).multiply(BR_RATE[R.ix(WS_IDX)-1]))).setScale(2, RoundingMode.HALF_UP);
    WS_O_TAX = R.dedit(WS_TAX, 2);
    WS_O_MARG = R.dedit(BR_RATE[R.ix(WS_IDX)-1], 4);
    System.out.println("TAX=" + WS_O_TAX.trim() + " BRACKET=" + R.dnumU(WS_IDX, 1) + " MARGINAL=" + WS_O_MARG.trim());
    return;
  }
}
