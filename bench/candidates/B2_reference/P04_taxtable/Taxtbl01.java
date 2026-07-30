
import java.math.BigDecimal;
import java.util.Scanner;

public class Taxtbl01 {
    public static void main(String[] a) {
        Scanner sc = new Scanner(System.in);
        String[] f = Cobol.unstring(sc.nextLine(), 2);
        BigDecimal inc = Cobol.trunc(new BigDecimal(Cobol.num(f[0])), 2);
        BigDecimal mult = f[1].equals("M") ? new BigDecimal("2.00") : new BigDecimal("1.00");

        BigDecimal[] ceil = new BigDecimal[5];
        BigDecimal[] rate = new BigDecimal[5];
        BigDecimal[] base = new BigDecimal[5];
        // Unrounded COMPUTE into PIC 9(9)V99 -> truncate 2dp (values exact anyway)
        ceil[0] = Cobol.trunc(new BigDecimal("11600.00").multiply(mult), 2);
        rate[0] = new BigDecimal("0.1000"); base[0] = new BigDecimal("0.00");
        ceil[1] = Cobol.trunc(new BigDecimal("47150.00").multiply(mult), 2);
        rate[1] = new BigDecimal("0.1200");
        base[1] = Cobol.trunc(new BigDecimal("1160.00").multiply(mult), 2);
        ceil[2] = Cobol.trunc(new BigDecimal("100525.00").multiply(mult), 2);
        rate[2] = new BigDecimal("0.2200");
        base[2] = Cobol.trunc(new BigDecimal("5426.00").multiply(mult), 2);
        ceil[3] = Cobol.trunc(new BigDecimal("191950.00").multiply(mult), 2);
        rate[3] = new BigDecimal("0.2400");
        base[3] = Cobol.trunc(new BigDecimal("17168.50").multiply(mult), 2);
        ceil[4] = new BigDecimal("999999999.00");
        rate[4] = new BigDecimal("0.3200");
        base[4] = Cobol.trunc(new BigDecimal("39110.50").multiply(mult), 2);

        // SET BI TO 1 / SEARCH (linear from current index) / AT END -> 5
        int idx = 5;
        for (int bi = 1; bi <= 5; bi++) {
            if (inc.compareTo(ceil[bi-1]) <= 0) { idx = bi; break; }
        }
        BigDecimal prev = (idx == 1) ? BigDecimal.ZERO : ceil[idx-2];
        BigDecimal tax = Cobol.rounded(
            base[idx-1].add(inc.subtract(prev).multiply(rate[idx-1])), 2);
        // WS-IDX PIC 9 -> single zero-padded digit; MARG PIC Z9.9999 trimmed
        System.out.println("TAX=" + Cobol.disp(tax,2)
            + " BRACKET=" + idx
            + " MARGINAL=" + rate[idx-1].setScale(4).toPlainString());
    }
}
