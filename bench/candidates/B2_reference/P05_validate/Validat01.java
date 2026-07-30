
import java.math.BigDecimal;
import java.util.Scanner;

public class Validat01 {
    public static void main(String[] a) {
        Scanner sc = new Scanner(System.in);
        String[] f = Cobol.unstring(sc.nextLine(), 3);
        String acct = f[0];
        BigDecimal amt = Cobol.trunc(new BigDecimal(Cobol.num(f[1])), 2);
        String state = f[2];

        int digits = 0;
        for (char c : acct.toCharArray()) if (c >= '0' && c <= '9') digits++;
        int len = acct.trim().length();

        int rc = 0; String msg = "OK";
        if (len != 8)                { rc = 101; msg = "BAD ACCT LENGTH"; }
        else if (digits != 8)        { rc = 102; msg = "ACCT NOT NUMERIC"; }
        else if (amt.signum() == 0)  { rc = 201; msg = "ZERO AMOUNT"; }
        else if (amt.signum() < 0)   { rc = 202; msg = "NEGATIVE AMOUNT"; }
        else if (amt.compareTo(new BigDecimal("10000.00")) > 0) { rc = 203; msg = "EXCEEDS LIMIT"; }
        else if (state.trim().isEmpty()) { rc = 301; msg = "MISSING STATE"; }
        System.out.println("RC=" + String.format("%03d", rc) + " MSG=" + msg);
    }
}
