
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

public class Payroll01 {
  public static void main(String[] args) {
    java.util.Scanner _sc = new java.util.Scanner(System.in);
    String WS_RAW = "";
    BigDecimal WS_HOURS = BigDecimal.ZERO;
    BigDecimal WS_RATE = BigDecimal.ZERO;
    BigDecimal WS_DEPS = BigDecimal.ZERO;
    BigDecimal WS_REG_HOURS = BigDecimal.ZERO;
    BigDecimal WS_OT_HOURS = BigDecimal.ZERO;
    BigDecimal WS_GROSS = BigDecimal.ZERO;
    BigDecimal WS_TAXABLE = BigDecimal.ZERO;
    BigDecimal WS_TAX = BigDecimal.ZERO;
    BigDecimal WS_NET = BigDecimal.ZERO;
    BigDecimal WS_DED = BigDecimal.ZERO;
    String WS_F1 = "";
    String WS_F2 = "";
    String WS_F3 = "";
    String WS_O_GROSS = "";
    String WS_O_NET = "";
    String WS_O_TAX = "";
    int BI = 1;
    WS_RAW = _sc.nextLine();
    String[] _p = R.unstring(WS_RAW, 3, ",");
    WS_F1 = _p[0];
    WS_F2 = _p[1];
    WS_F3 = _p[2];
    WS_HOURS = (new BigDecimal(WS_F1.trim())).setScale(2, RoundingMode.DOWN);
    WS_RATE = (new BigDecimal(WS_F2.trim())).setScale(2, RoundingMode.DOWN);
    WS_DEPS = (new BigDecimal(WS_F3.trim())).setScale(0, RoundingMode.DOWN);
    if ((WS_HOURS).compareTo(new BigDecimal("40.00")) > 0) {
      WS_REG_HOURS = (new BigDecimal("40.00")).setScale(2, RoundingMode.DOWN);
      WS_OT_HOURS = (WS_HOURS.subtract(new BigDecimal("40.00"))).setScale(2, RoundingMode.DOWN);
    } else {
      WS_REG_HOURS = (WS_HOURS).setScale(2, RoundingMode.DOWN);
      WS_OT_HOURS = (BigDecimal.ZERO).setScale(2, RoundingMode.DOWN);
    }
    WS_GROSS = (WS_REG_HOURS.multiply(WS_RATE).add(WS_OT_HOURS.multiply(WS_RATE).multiply(new BigDecimal("1.5")))).setScale(2, RoundingMode.HALF_UP);
    WS_DED = (WS_DEPS.multiply(new BigDecimal("76.92"))).setScale(2, RoundingMode.DOWN);
    WS_TAXABLE = (WS_GROSS.subtract(WS_DED)).setScale(2, RoundingMode.DOWN);
    if ((WS_TAXABLE).compareTo(new BigDecimal("0")) < 0) {
      WS_TAXABLE = (BigDecimal.ZERO).setScale(2, RoundingMode.DOWN);
    }
    if ((WS_TAXABLE).compareTo(new BigDecimal("500.00")) <= 0) {
      WS_TAX = (WS_TAXABLE.multiply(new BigDecimal("0.10"))).setScale(2, RoundingMode.HALF_UP);
    } else if ((WS_TAXABLE).compareTo(new BigDecimal("1500.00")) <= 0) {
      WS_TAX = (new BigDecimal("50.00").add(WS_TAXABLE.subtract(new BigDecimal("500.00")).multiply(new BigDecimal("0.22")))).setScale(2, RoundingMode.HALF_UP);
    } else {
      WS_TAX = (new BigDecimal("270.00").add(WS_TAXABLE.subtract(new BigDecimal("1500.00")).multiply(new BigDecimal("0.32")))).setScale(2, RoundingMode.HALF_UP);
    }
    WS_NET = (WS_GROSS.subtract(WS_TAX)).setScale(2, RoundingMode.DOWN);
    WS_O_GROSS = R.dedit(WS_GROSS, 2);
    WS_O_NET = R.dedit(WS_NET, 2);
    WS_O_TAX = R.dedit(WS_TAX, 2);
    System.out.println("GROSS=" + WS_O_GROSS.trim() + " NET=" + WS_O_NET.trim() + " TAX=" + WS_O_TAX.trim());
    return;
  }
}
