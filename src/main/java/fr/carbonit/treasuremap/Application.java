package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.treasurehunt.TreasureHunt;
import fr.carbonit.treasuremap.treasurehunt.TreasureHuntInputManagement;
import fr.carbonit.treasuremap.treasurehunt.TreasureHuntOutputManagement;

public class Application {
    public static void main(String[] args) {

        TreasureHuntInputManagement treasureHuntInputManagement = new TreasureHuntInputManagement(
                args.length > 0 ? args[0] : "src/main/resources/input.txt");
        TreasureHunt                 treasureHunt                 = treasureHuntInputManagement.read();
        TreasureHuntOutputManagement treasureHuntOutputManagement = new TreasureHuntOutputManagement(treasureHunt);

        treasureHuntOutputManagement.write();
    }
}
