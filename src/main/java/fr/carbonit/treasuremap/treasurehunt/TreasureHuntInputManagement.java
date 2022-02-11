package fr.carbonit.treasuremap.treasurehunt;

import fr.carbonit.treasuremap.adventurer.AdventurerOrientation;
import fr.carbonit.treasuremap.adventurer.PlainAdventurer;
import fr.carbonit.treasuremap.exception.DataValidity;
import fr.carbonit.treasuremap.exception.TreasureHuntFile;

import java.awt.*;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TreasureHuntInputManagement {
    private final String       filePath;
    private       TreasureHunt treasureHunt;

    public TreasureHuntInputManagement(String filePath) {
        this.filePath = filePath;
    }

    public TreasureHunt read()
            throws
            DataValidity {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath != null ? filePath : ""))) {
            String line;
            while ((line = reader.readLine()) != null) {
                parseLine(line);
            }
        } catch (IOException ioException) {
            throw new TreasureHuntFile("Cannot read input file", ioException.getCause());
        }
        return treasureHunt;
    }

    private void parseLine(String line) {
        String[] split = line.split(" - ");
        switch (split[0]) {
            case "C":
                // only when this is the first line for C (ignoring the following ones)
                if (treasureHunt == null) {
                    treasureHunt = new TreasureHunt(Integer.valueOf(split[1]), Integer.valueOf(split[2]));
                }
                break;
            case "M":
                treasureHunt.putMountainOnTreasureMap(Integer.valueOf(split[1]), Integer.valueOf(split[2]));
                break;
            case "T":
                treasureHunt.putTreasureOnTreasureMap(Integer.valueOf(split[1]),
                                                      Integer.valueOf(split[2]),
                                                      Integer.valueOf(split[2]));
                break;
            case "A":
                AdventurerOrientation adventurerOrientation = readAdventurerOrientation(split[4]);
                List<String> actions = readAdventurerActions(split[5]);
                treasureHunt.putAdventurerOnTreasureMap(new PlainAdventurer(split[1],
                                                                            new Point(Integer.valueOf(split[2]),
                                                                                      Integer.valueOf(split[3])),
                                                                            adventurerOrientation,
                                                                            actions));
                break;
            default:
                break;
        }
    }

    private List<String> readAdventurerActions(String s) {
        // gets "AAAA" as modifiable list
        List<String> actions = new ArrayList<>(Arrays.asList((s.split(""))));

        if (actions.stream()
                   .allMatch(string -> string.equals("A") || string.equals("D") || string.equals("G"))) {
            return actions;
        }
        throw new DataValidity("Unsupported Adventurer Actions");
    }

    private AdventurerOrientation readAdventurerOrientation(String name) {
        if (name.equals("N") || name.equals("S") || name.equals("E") || name.equals("O")) {
            return AdventurerOrientation.valueOf(name);
        }
        throw new DataValidity("Unsupported Orientation");
    }
}
