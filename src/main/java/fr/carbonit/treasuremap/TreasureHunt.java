package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.adventurer.Adventurer;
import fr.carbonit.treasuremap.cell.MapCell;
import fr.carbonit.treasuremap.cell.MountainCell;
import fr.carbonit.treasuremap.cell.PlainCell;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class TreasureHunt {
    private final Integer width;
    private final Integer height;

    private final MapCell[][] treasureMap;

    private final List<Adventurer> adventurerList = new ArrayList<>();

    public TreasureHunt(Integer width, Integer height) {
        this.width  = width;
        this.height = height;

        this.treasureMap = new MapCell[width][height];

        initializeMapWithPlainCells();
    }

    private void initializeMapWithPlainCells() {
        for (int mapWidth = 0; mapWidth < width; mapWidth++) {
            for (int mapHeight = 0; mapHeight < height; mapHeight++) {
                treasureMap[mapWidth][mapHeight] = new PlainCell(mapWidth, mapHeight);
            }
        }
    }

    public void putMountainOnTreasureMap(Integer xCoordinate, Integer yCoordinate) {
        this.treasureMap[xCoordinate][yCoordinate] = new MountainCell(xCoordinate, yCoordinate);
    }

    public void putTreasureOnTreasureMap(int xCoordinate, int yCoordinate) {
        this.treasureMap[xCoordinate][yCoordinate].addTreasure();
    }

    public void putAdventurerOnTreasureMap(Adventurer adventurer) {
        this.treasureMap[adventurer.getCoordinates().x][adventurer.getCoordinates().y].updateOccupiedStatus();
        this.adventurerList.add(adventurer);
    }

    public void simulate() {
        do {
            adventurerList.forEach(adventurer -> {
                if (Boolean.FALSE.equals(adventurer.hasFinished())) {
                    adventurer.move(treasureMap);
                }
            });
        } while (!adventurerList.stream()
                                .allMatch(Adventurer::hasFinished));
    }

    public String getSimulationResult() {
        StringBuilder mapDefinition = new StringBuilder("C - " + width + " - " + height + "\n");

        for (int mapWidth = 0; mapWidth < width; mapWidth++) {
            for (int mapHeight = 0; mapHeight < height; mapHeight++) {
                MapCell mapCell = treasureMap[mapWidth][mapHeight];
                if (Boolean.TRUE.equals(mapCell.isMountain())) {
                    mapDefinition.append(mapCell)
                                 .append("\n");
                }
            }
        }

        for (int mapWidth = 0; mapWidth < width; mapWidth++) {
            for (int mapHeight = 0; mapHeight < height; mapHeight++) {
                MapCell mapCell   = treasureMap[mapWidth][mapHeight];
                Integer treasures = mapCell.getTreasures();
                if (treasures > 0) {
                    mapDefinition.append("T - ")
                                 .append(mapWidth)
                                 .append(" - ")
                                 .append(mapHeight)
                                 .append(" - ")
                                 .append(treasures)
                                 .append("\n");
                }
            }
        }

        mapDefinition.append(adventurerList.stream()
                                           .map(Objects::toString)
                                           .collect(Collectors.joining("\n")));

        deleteEmptyLine(mapDefinition);
        return mapDefinition.toString();
    }

    private void deleteEmptyLine(StringBuilder mapDefinition) {
        if (mapDefinition.lastIndexOf("\n") == mapDefinition.length() - 1) {
            mapDefinition.deleteCharAt(mapDefinition.length() - 1);
        }
    }
}
