
import java.math.BigDecimal;
import java.math.RoundingMode;

final class R {
    /* edited picture then FUNCTION TRIM -> plain decimal at edit scale */
    static String dedit(BigDecimal v, int dec) {
        return v.setScale(dec, RoundingMode.DOWN).toPlainString();
    }
    static boolean eq(String a, String b) { return rtrim(a).equals(rtrim(b)); }
    static String rtrim(String s) {
        int e = s.length();
        while (e > 0 && s.charAt(e-1) == ' ') e--;
        return s.substring(0, e);
    }
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

public class Exitflw01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    String WS_F1 = "";
    String WS_F2 = "";
    BigDecimal WS_N = BigDecimal.ZERO;
    BigDecimal WS_I = BigDecimal.ZERO;
    BigDecimal WS_SUM = BigDecimal.ZERO;
    BigDecimal WS_TICK = BigDecimal.ZERO;
    String WS_MODE = "";
    String WS_OUT_SUM = "";
    String WS_OUT_TICK = "";
    int BI = 1;
    int RETURN_CODE = 0;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 2, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_N = (new BigDecimal(WS_F1.trim())).setScale(0, RoundingMode.DOWN);
    WS_MODE = R.take(WS_F2.trim(), 1);
    WS_SUM = (new BigDecimal("0")).setScale(0, RoundingMode.DOWN);
    WS_TICK = (new BigDecimal("0")).setScale(0, RoundingMode.DOWN);
    for (WS_I = new BigDecimal("1"); !((WS_I).compareTo(WS_N) > 0); WS_I = WS_I.add(new BigDecimal("1"))) {
      if ((WS_I).compareTo(new BigDecimal("9000")) > 0) {
      } else {
        WS_SUM = (WS_SUM.add(WS_I)).setScale(0, RoundingMode.DOWN);
      }
      WS_TICK = (WS_TICK.add(new BigDecimal("1"))).setScale(0, RoundingMode.DOWN);
    }
    if (R.eq(WS_MODE, "G")) {
    } else {
      WS_TICK = (WS_TICK.add(new BigDecimal("1"))).setScale(0, RoundingMode.DOWN);
    }
    WS_OUT_SUM = R.dedit(WS_SUM, 0);
    WS_OUT_TICK = R.dedit(WS_TICK, 0);
    System.out.println("TICK=" + WS_OUT_TICK.trim() + " SUM=" + WS_OUT_SUM.trim() + " MODE=" + WS_MODE);
    if (R.eq(WS_MODE, "P")) {
      System.out.println("STATE=RETURNED");
      RETURN_CODE = 0;
      System.exit(RETURN_CODE);
    }
    if (R.eq(WS_MODE, "E")) {
      System.out.println("STATE=FAULT");
      RETURN_CODE = 8;
      System.exit(RETURN_CODE);
    }
    if (R.eq(WS_MODE, "W")) {
      System.out.println("STATE=WARN");
      RETURN_CODE = 4;
      System.exit(RETURN_CODE);
    }
    System.out.println("STATE=DONE");
    System.exit(RETURN_CODE);
  }
}
