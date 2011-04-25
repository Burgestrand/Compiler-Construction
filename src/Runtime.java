import java.util.Scanner;

class Runtime
{
  public static void printInt(int n) { System.out.println(n); }
  public static void printDouble(double x) { System.out.println(x); }
  public static void printString(String s) { System.out.println(s); }
  
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