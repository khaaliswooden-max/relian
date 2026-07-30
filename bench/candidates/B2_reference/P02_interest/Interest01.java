
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Scanner;

public class Interest01 {
    public static void main(String[] a) {
        Scanner sc = new Scanner(System.in);
        String[] f = Cobol.unstring(sc.nextLine(), 4);
        BigDecimal prin  = Cobol.trunc(new BigDecimal(Cobol.num(f[0])), 2);
        BigDecimal rate  = Cobol.trunc(new BigDecimal(Cobol.num(f[1])), 4);
        int years        = new BigDecimal(Cobol.num(f[2])).intValue();
        BigDecimal add   = Cobol.trunc(new BigDecimal(Cobol.num(f[3])), 2);

        // COMPUTE WS-MRATE = WS-RATE / 100 / 12  (unROUNDED -> truncate to
        // the receiving PIC 9(3)V9(8) scale of 8dp). High-precision
        // intermediate, single truncation on store -- GnuCOBOL semantics.
        BigDecimal mrate = rate.divide(new BigDecimal(1200), 20, RoundingMode.DOWN)
                               .setScale(8, RoundingMode.DOWN);
        int months = years * 12;
        BigDecimal bal = prin, totadd = BigDecimal.ZERO;
        BigDecimal onePlus = BigDecimal.ONE.add(mrate);
        for (int i = 1; i <= months; i++) {
            // COMPUTE WS-BAL ROUNDED = WS-BAL * (1 + WS-MRATE) -> HALF_UP 2dp
            bal = Cobol.rounded(bal.multiply(onePlus), 2);
            bal = bal.add(add);
            totadd = totadd.add(add);
        }
        BigDecimal interest = bal.subtract(prin).subtract(totadd);
        System.out.println("BAL=" + Cobol.disp(bal,2) + " INT=" + Cobol.disp(interest,2));
    }
}
