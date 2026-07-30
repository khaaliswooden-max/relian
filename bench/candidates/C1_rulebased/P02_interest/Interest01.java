
import java.math.BigDecimal;
import java.math.RoundingMode;

final class R {
    /* edited picture then FUNCTION TRIM -> plain decimal at edit scale */
    static String dedit(BigDecimal v, int dec) {
        return v.setScale(dec, RoundingMode.DOWN).toPlainString();
    }
    static String[] unstring(String line, int n, String delim) {
        String[] out = new String[n];
        java.util.Arrays.fill(out, "");
        String[] p = line.split(java.util.regex.Pattern.quote(delim), -1);
        System.arraycopy(p, 0, out, 0, Math.min(n, p.length));
        return out;
    }
}

public class Interest01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    String WS_F1 = "";
    String WS_F2 = "";
    String WS_F3 = "";
    String WS_F4 = "";
    BigDecimal WS_PRIN = BigDecimal.ZERO;
    BigDecimal WS_RATE = BigDecimal.ZERO;
    BigDecimal WS_YEARS = BigDecimal.ZERO;
    BigDecimal WS_ADD = BigDecimal.ZERO;
    BigDecimal WS_BAL = BigDecimal.ZERO;
    BigDecimal WS_MRATE = BigDecimal.ZERO;
    BigDecimal WS_INT = BigDecimal.ZERO;
    BigDecimal WS_TOTADD = BigDecimal.ZERO;
    BigDecimal I = BigDecimal.ZERO;
    BigDecimal WS_MONTHS = BigDecimal.ZERO;
    String WS_O_BAL = "";
    String WS_O_INT = "";
    int BI = 1;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 4, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_F3 = _p[2];
    WS_F4 = _p[3];
    WS_PRIN = (new BigDecimal(WS_F1.trim())).setScale(2, RoundingMode.DOWN);
    WS_RATE = (new BigDecimal(WS_F2.trim())).setScale(4, RoundingMode.DOWN);
    WS_YEARS = (new BigDecimal(WS_F3.trim())).setScale(0, RoundingMode.DOWN);
    WS_ADD = (new BigDecimal(WS_F4.trim())).setScale(2, RoundingMode.DOWN);
    WS_MRATE = (WS_RATE.divide(new BigDecimal("100"), 20, RoundingMode.DOWN).divide(new BigDecimal("12"), 20, RoundingMode.DOWN)).setScale(8, RoundingMode.DOWN);
    WS_MONTHS = (WS_YEARS.multiply(new BigDecimal("12"))).setScale(0, RoundingMode.DOWN);
    WS_BAL = (WS_PRIN).setScale(2, RoundingMode.DOWN);
    WS_TOTADD = (BigDecimal.ZERO).setScale(2, RoundingMode.DOWN);
    for (I = new BigDecimal("1"); !((I).compareTo(WS_MONTHS) > 0); I = I.add(new BigDecimal("1"))) {
      WS_BAL = (WS_BAL.multiply(new BigDecimal("1").add(WS_MRATE))).setScale(2, RoundingMode.HALF_UP);
      WS_BAL = (WS_BAL.add(WS_ADD)).setScale(2, RoundingMode.DOWN);
      WS_TOTADD = (WS_TOTADD.add(WS_ADD)).setScale(2, RoundingMode.DOWN);
    }
    WS_INT = (WS_BAL.subtract(WS_PRIN).subtract(WS_TOTADD)).setScale(2, RoundingMode.DOWN);
    WS_O_BAL = R.dedit(WS_BAL, 2);
    WS_O_INT = R.dedit(WS_INT, 2);
    System.out.println("BAL=" + WS_O_BAL.trim() + " INT=" + WS_O_INT.trim());
    return;
  }
}
