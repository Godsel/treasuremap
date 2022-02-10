package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.exception.OutputFile;

import java.io.FileWriter;
import java.io.IOException;

public class TreasureHunterOutputManagement {
    private final TreasureHunt treasureHunt;

    public TreasureHunterOutputManagement(TreasureHunt treasureHunt) {
        this.treasureHunt = treasureHunt;
    }

    public void write() {
        try (FileWriter fileWriter = new FileWriter("src/main/resources/output.txt")) {
            fileWriter.write(treasureHunt.getSimulationResult());
        } catch (IOException ioException) {
            throw new OutputFile("Error when writing output file", ioException.getCause());
        }
    }
}
