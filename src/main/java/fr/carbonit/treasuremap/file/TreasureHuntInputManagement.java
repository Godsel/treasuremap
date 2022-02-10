package fr.carbonit.treasuremap.file;

import fr.carbonit.treasuremap.TreasureHunt;
import fr.carbonit.treasuremap.exception.TreasureHuntFile;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class TreasureHuntInputManagement {
    private final String       filePath;
    private       TreasureHunt treasureHunt;

    public TreasureHuntInputManagement(String filePath) {
        this.filePath = filePath;
    }

    public TreasureHunt read()
            throws
            RuntimeException {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath != null ? filePath : ""));
        } catch (FileNotFoundException fileNotFoundException) {
            throw new TreasureHuntFile("None input file, quitting", fileNotFoundException.getCause());
        }
        return treasureHunt;
    }
}
