package fr.carbonit.treasuremap.file;

import fr.carbonit.treasuremap.TreasureHunt;
import fr.carbonit.treasuremap.exception.TreasureHuntFile;

import java.io.FileWriter;
import java.io.IOException;

public class TreasureHuntOutputManagement {
    private final TreasureHunt treasureHunt;

    public TreasureHuntOutputManagement(TreasureHunt treasureHunt) {
        this.treasureHunt = treasureHunt;
    }

    public void write() {
        treasureHunt.simulate();
        try (FileWriter fileWriter = new FileWriter("src/main/resources/output.txt")) {
            fileWriter.write(treasureHunt.getSimulationResult());
        } catch (IOException ioException) {
            throw new TreasureHuntFile("Error when writing output file", ioException.getCause());
        }
    }
}
