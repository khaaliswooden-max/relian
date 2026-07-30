
import java.math.BigDecimal;
import java.math.RoundingMode;

final class R {
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

public class Validat01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    String WS_F1 = "";
    String WS_F2 = "";
    String WS_F3 = "";
    String WS_ACCT = "";
    BigDecimal WS_AMT = BigDecimal.ZERO;
    String WS_STATE = "";
    BigDecimal WS_RC = BigDecimal.ZERO;
    String WS_MSG = "";
    BigDecimal WS_DIGITS = BigDecimal.ZERO;
    BigDecimal WS_LEN = BigDecimal.ZERO;
    int BI = 1;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 3, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_F3 = _p[2];
    WS_ACCT = R.take(WS_F1.trim(), 20);
    WS_AMT = (new BigDecimal(WS_F2.trim())).setScale(2, RoundingMode.DOWN);
    WS_STATE = R.take(WS_F3.trim(), 2);
    WS_DIGITS = (BigDecimal.ZERO).setScale(0, RoundingMode.DOWN);
    { int _c = 0; for (char _ch : WS_ACCT.toCharArray()) if ("0123456789".indexOf(_ch) >= 0) _c++; WS_DIGITS = new BigDecimal(_c); }
    WS_LEN = (new BigDecimal(WS_ACCT.trim().length())).setScale(0, RoundingMode.DOWN);
    WS_RC = (new BigDecimal("0")).setScale(0, RoundingMode.DOWN);
    WS_MSG = "OK";
    if ((WS_LEN).compareTo(new BigDecimal("8")) != 0) {
      WS_RC = (new BigDecimal("101")).setScale(0, RoundingMode.DOWN);
      WS_MSG = "BAD ACCT LENGTH";
    } else if ((WS_DIGITS).compareTo(new BigDecimal("8")) != 0) {
      WS_RC = (new BigDecimal("102")).setScale(0, RoundingMode.DOWN);
      WS_MSG = "ACCT NOT NUMERIC";
    } else if ((WS_AMT).compareTo(new BigDecimal("0")) == 0) {
      WS_RC = (new BigDecimal("201")).setScale(0, RoundingMode.DOWN);
      WS_MSG = "ZERO AMOUNT";
    } else if ((WS_AMT).compareTo(new BigDecimal("0")) < 0) {
      WS_RC = (new BigDecimal("202")).setScale(0, RoundingMode.DOWN);
      WS_MSG = "NEGATIVE AMOUNT";
    } else if ((WS_AMT).compareTo(new BigDecimal("10000.00")) > 0) {
      WS_RC = (new BigDecimal("203")).setScale(0, RoundingMode.DOWN);
      WS_MSG = "EXCEEDS LIMIT";
    } else if (R.eq(WS_STATE, "  ")) {
      WS_RC = (new BigDecimal("301")).setScale(0, RoundingMode.DOWN);
      WS_MSG = "MISSING STATE";
    }
    System.out.println("RC=" + R.dnumU(WS_RC, 3) + " MSG=" + WS_MSG.trim());
    return;
  }
}
