package fr.carbonit.treasuremap.treasurehunt;

import fr.carbonit.treasuremap.adventurer.Adventurer;
import fr.carbonit.treasuremap.cell.MapCell;
import fr.carbonit.treasuremap.cell.MountainCell;
import fr.carbonit.treasuremap.cell.PlainCell;
import fr.carbonit.treasuremap.exception.DataValidity;

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

    public void putMountainOnTreasureMap(Integer xCoordinate, Integer yCoordinate)
            throws
            DataValidity {
        verifyCoordinates(xCoordinate, yCoordinate);
        this.treasureMap[xCoordinate][yCoordinate] = new MountainCell(xCoordinate, yCoordinate);
    }

    private void verifyCoordinates(Integer xCoordinate, Integer yCoordinate) {
        if ((xCoordinate > this.treasureMap.length || xCoordinate < 0) ||
            (yCoordinate > this.treasureMap[0].length || yCoordinate < 0)) {
            throw new DataValidity("Object cannot be placed due to outbounds coordinates");
        }
    }

    public void putTreasureOnTreasureMap(int xCoordinate, int yCoordinate, Integer number)
            throws
            DataValidity {
        verifyCoordinates(xCoordinate, yCoordinate);
        for (int treasuresNumber = 0; treasuresNumber < number; treasuresNumber++) {
            this.treasureMap[xCoordinate][yCoordinate].addTreasure();
        }
    }

    public void putAdventurerOnTreasureMap(Adventurer adventurer)
            throws
            DataValidity {
        verifyCoordinates(adventurer.getCoordinates().x, adventurer.getCoordinates().y);
        MapCell mapCell = this.treasureMap[adventurer.getCoordinates().x][adventurer.getCoordinates().y];
        verifyCollisionWithExistingAdventurer(mapCell);
        mapCell.updateOccupiedStatus();
        this.adventurerList.add(adventurer);
    }

    private void verifyCollisionWithExistingAdventurer(MapCell mapCell) {
        if (Boolean.TRUE.equals(mapCell.isOccupied())) {
            throw new DataValidity("Adventurer cannot start on the same location than an other one");
        }
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
        StringBuilder simulationResult = new StringBuilder();
        writeMapDefinition(simulationResult);

        writeMountainDefinitions(simulationResult);

        writeTreasureDefinitions(simulationResult);

        writeAdventurerDefinitions(simulationResult);

        // little hack when none adventurer
        deleteEmptyLine(simulationResult);
        return simulationResult.toString();
    }

    private void writeAdventurerDefinitions(StringBuilder mapDefinition) {
        mapDefinition.append(adventurerList.stream()
                                           .map(Objects::toString)
                                           .collect(Collectors.joining("\n")));
    }

    private void writeTreasureDefinitions(StringBuilder mapDefinition) {
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
    }

    private void writeMountainDefinitions(StringBuilder mapDefinition) {
        for (int mapWidth = 0; mapWidth < width; mapWidth++) {
            for (int mapHeight = 0; mapHeight < height; mapHeight++) {
                MapCell mapCell = treasureMap[mapWidth][mapHeight];
                if (Boolean.TRUE.equals(mapCell.isMountain())) {
                    mapDefinition.append(mapCell)
                                 .append("\n");
                }
            }
        }
    }

    private void writeMapDefinition(StringBuilder mapDefinition) {
        mapDefinition.append("C - ")
                     .append(width)
                     .append(" - ")
                     .append(height)
                     .append("\n");
    }

    private void deleteEmptyLine(StringBuilder mapDefinition) {
        if (mapDefinition.lastIndexOf("\n") == mapDefinition.length() - 1) {
            mapDefinition.deleteCharAt(mapDefinition.length() - 1);
        }
    }
}
