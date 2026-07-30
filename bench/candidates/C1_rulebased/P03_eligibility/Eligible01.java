
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

public class Eligible01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    String WS_F1 = "";
    String WS_F2 = "";
    String WS_F3 = "";
    String WS_F4 = "";
    BigDecimal WS_INCOME = BigDecimal.ZERO;
    BigDecimal WS_SIZE = BigDecimal.ZERO;
    BigDecimal WS_AGE = BigDecimal.ZERO;
    String WS_DIS = "";
    BigDecimal WS_FPL = BigDecimal.ZERO;
    BigDecimal WS_PCT = BigDecimal.ZERO;
    String WS_STATUS = "";
    String WS_CAT = "";
    String WS_O_PCT = "";
    int BI = 1;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 4, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_F3 = _p[2];
    WS_F4 = _p[3];
    WS_INCOME = (new BigDecimal(WS_F1.trim())).setScale(2, RoundingMode.DOWN);
    WS_SIZE = (new BigDecimal(WS_F2.trim())).setScale(0, RoundingMode.DOWN);
    WS_AGE = (new BigDecimal(WS_F3.trim())).setScale(0, RoundingMode.DOWN);
    WS_DIS = R.take(WS_F4.trim(), 1);
    WS_FPL = (new BigDecimal("15060").add(WS_SIZE.subtract(new BigDecimal("1")).multiply(new BigDecimal("5380")))).setScale(2, RoundingMode.DOWN);
    WS_PCT = (WS_INCOME.divide(WS_FPL, 20, RoundingMode.DOWN).multiply(new BigDecimal("100"))).setScale(2, RoundingMode.HALF_UP);
    WS_STATUS = "DENIED";
    WS_CAT = "NONE";
    if ((WS_AGE).compareTo(new BigDecimal("19")) < 0 && (WS_PCT).compareTo(new BigDecimal("212.00")) <= 0) {
      WS_STATUS = "APPROVED";
      WS_CAT = "CHILD";
    } else if (R.eq(WS_DIS, "Y") && (WS_PCT).compareTo(new BigDecimal("200.00")) <= 0) {
      WS_STATUS = "APPROVED";
      WS_CAT = "DISABILITY";
    } else if ((WS_AGE).compareTo(new BigDecimal("65")) >= 0 && (WS_PCT).compareTo(new BigDecimal("150.00")) <= 0) {
      WS_STATUS = "APPROVED";
      WS_CAT = "AGED";
    } else if ((WS_PCT).compareTo(new BigDecimal("138.00")) <= 0) {
      WS_STATUS = "APPROVED";
      WS_CAT = "EXPANSION";
    } else if ((WS_PCT).compareTo(new BigDecimal("200.00")) <= 0) {
      WS_STATUS = "PENDING";
      WS_CAT = "REVIEW";
    }
    WS_O_PCT = R.dedit(WS_PCT, 2);
    System.out.println("STATUS=" + WS_STATUS.trim() + " CATEGORY=" + WS_CAT.trim() + " FPLPCT=" + WS_O_PCT.trim());
    return;
  }
}
