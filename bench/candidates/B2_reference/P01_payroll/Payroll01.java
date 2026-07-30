
import java.math.BigDecimal;
import java.util.Scanner;

public class Payroll01 {
    public static void main(String[] a) {
        Scanner sc = new Scanner(System.in);
        String[] f = Cobol.unstring(sc.nextLine(), 3);
        BigDecimal hours = Cobol.trunc(new BigDecimal(Cobol.num(f[0])), 2);
        BigDecimal rate  = Cobol.trunc(new BigDecimal(Cobol.num(f[1])), 2);
        int deps = new BigDecimal(Cobol.num(f[2])).intValue();

        BigDecimal reg, ot;
        if (hours.compareTo(new BigDecimal("40.00")) > 0) {
            reg = new BigDecimal("40.00");
            ot = hours.subtract(new BigDecimal("40.00"));
        } else { reg = hours; ot = BigDecimal.ZERO; }

        BigDecimal gross = Cobol.rounded(
            reg.multiply(rate).add(ot.multiply(rate).multiply(new BigDecimal("1.5"))), 2);
        BigDecimal ded = Cobol.trunc(new BigDecimal(deps).multiply(new BigDecimal("76.92")), 2);
        BigDecimal taxable = gross.subtract(ded);
        if (taxable.signum() < 0) taxable = BigDecimal.ZERO;

        BigDecimal tax;
        if (taxable.compareTo(new BigDecimal("500.00")) <= 0) {
            tax = Cobol.rounded(taxable.multiply(new BigDecimal("0.10")), 2);
        } else if (taxable.compareTo(new BigDecimal("1500.00")) <= 0) {
            tax = Cobol.rounded(new BigDecimal("50.00").add(
                taxable.subtract(new BigDecimal("500.00")).multiply(new BigDecimal("0.22"))), 2);
        } else {
            tax = Cobol.rounded(new BigDecimal("270.00").add(
                taxable.subtract(new BigDecimal("1500.00")).multiply(new BigDecimal("0.32"))), 2);
        }
        BigDecimal net = gross.subtract(tax);
        System.out.println("GROSS=" + Cobol.disp(gross,2) + " NET=" + Cobol.disp(net,2)
            + " TAX=" + Cobol.disp(tax,2));
    }
}
