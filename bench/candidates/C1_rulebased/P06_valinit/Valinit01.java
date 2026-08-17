
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

public class Valinit01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    String WS_F1 = "";
    String WS_F2 = "";
    BigDecimal WS_DELTA = BigDecimal.ZERO;
    BigDecimal WS_BAL = new BigDecimal("1234.56");
    BigDecimal WS_RATE = new BigDecimal("0.0725");
    String WS_NAME = "ACME LTD  ";
    String WS_G1 = "AB";
    BigDecimal WS_G2 = new BigDecimal("12");
    String WS_G3 = "CD";
    String WS_CODE = "";
    String WS_OUT_BAL = "";
    String WS_OUT_RATE = "";
    int BI = 1;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 2, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_DELTA = (new BigDecimal(WS_F1.trim())).setScale(2, RoundingMode.DOWN);
    WS_CODE = R.take(WS_F2.trim(), 1);
    WS_BAL = (WS_BAL.add(WS_DELTA)).setScale(2, RoundingMode.DOWN);
    WS_OUT_BAL = R.dedit(WS_BAL, 2);
    WS_OUT_RATE = R.dedit(WS_RATE, 4);
    if (R.eq(WS_CODE, "A")) {
      System.out.println("STATE=ACTIVE BAL=" + WS_OUT_BAL.trim());
    } else {
      if ((R.eq(WS_CODE, "C") || R.eq(WS_CODE, "X"))) {
        System.out.println("STATE=CLOSED BAL=" + WS_OUT_BAL.trim());
      } else {
        System.out.println("STATE=OTHER BAL=" + WS_OUT_BAL.trim());
      }
    }
    System.out.println("RATE=" + WS_OUT_RATE + " NAME=" + WS_NAME.trim() + " G1=" + WS_G1 + " G2=" + R.dnumU(WS_G2, 2) + " G3=" + WS_G3);
    return;
  }
}
