
import java.math.BigDecimal;
import java.util.Scanner;

public class Eligible01 {
    public static void main(String[] a) {
        Scanner sc = new Scanner(System.in);
        String[] f = Cobol.unstring(sc.nextLine(), 4);
        BigDecimal income = Cobol.trunc(new BigDecimal(Cobol.num(f[0])), 2);
        int size = new BigDecimal(Cobol.num(f[1])).intValue();
        int age  = new BigDecimal(Cobol.num(f[2])).intValue();
        boolean disabled = f[3].equals("Y");

        BigDecimal fpl = new BigDecimal(15060).add(
            new BigDecimal(size - 1).multiply(new BigDecimal(5380)));
        BigDecimal pct = Cobol.rounded(
            income.divide(fpl, 10, java.math.RoundingMode.DOWN)
                  .multiply(new BigDecimal(100)), 2);

        String status = "DENIED", cat = "NONE";
        if (age < 19 && pct.compareTo(new BigDecimal("212.00")) <= 0) {
            status = "APPROVED"; cat = "CHILD";
        } else if (disabled && pct.compareTo(new BigDecimal("200.00")) <= 0) {
            status = "APPROVED"; cat = "DISABILITY";
        } else if (age >= 65 && pct.compareTo(new BigDecimal("150.00")) <= 0) {
            status = "APPROVED"; cat = "AGED";
        } else if (pct.compareTo(new BigDecimal("138.00")) <= 0) {
            status = "APPROVED"; cat = "EXPANSION";
        } else if (pct.compareTo(new BigDecimal("200.00")) <= 0) {
            status = "PENDING"; cat = "REVIEW";
        }
        System.out.println("STATUS=" + status + " CATEGORY=" + cat
            + " FPLPCT=" + Cobol.disp(pct,2));
    }
}
