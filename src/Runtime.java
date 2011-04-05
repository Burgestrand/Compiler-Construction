import java.util.Scanner;

class Runtime
{
  public static void printInt(int n) { System.out.print(n); }
  public static void printDouble(double x) { System.out.print(x); }
  public static void printString(String s) { System.out.print(s); }
  
  public static int readInt() { return getScanner().nextInt(); }
  public static double readDouble() { return getScanner().nextDouble(); }
  
  /**
   * Retrieve the internal scanner object.
   */
  private static Scanner scanner = null;
  private static Scanner getScanner()
  {
    if (scanner == null) scanner = new Scanner(System.in);
    return scanner;
  }
}