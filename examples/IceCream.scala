object IceCream {
 /** helper function definition */
  def askFlavor(numScoop: Int, wanted: Int): Unit = {
    if (numScoop == wanted) {
      Std.printString("Here you go!");
      Std.printString("........");
      Std.printString("     Q");
      Std.printString("   (   )");
      Std.printString("  {,~~~,}");
      Std.printString("   \MMM/");
      Std.printString("    \W/");
      Std.printString("     V")
    }
    else {
      val suffix : String = (numScoop+1) match {
        case 3 => "rd"
        case 2 => "nd"
        case 1 => "st"
        case _ => "th"
      };
      Std.printString("What flavor would you like for your " ++ Std.intToString(numScoop+1) ++ suffix ++ " scoop?");
      val flavor : String = Std.readString();
      Std.printString("Sure, one scoop of " ++ flavor);
      Std.printString("........");
      askFlavor(numScoop+1, wanted)
    }
  }
  
  /** program starts here */
  Std.printString("How many scoops would you like?");
  val scoops : Int = Std.readInt();
  if (3 < scoops) { 
    Std.printString("I am afraid we cannot fit all of them in one cone, don't be too greedy!")
  }
  else {
    askFlavor(0, scoops)
  }
	
}
