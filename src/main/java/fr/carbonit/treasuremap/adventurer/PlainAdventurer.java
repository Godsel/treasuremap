package fr.carbonit.treasuremap.adventurer;

import fr.carbonit.treasuremap.cell.MapCell;

import java.awt.*;
import java.util.List;
import java.util.Objects;

public class PlainAdventurer
        extends Adventurer {

    public PlainAdventurer(String name, Point coords, AdventurerOrientation adventurerOrientation,
                           List<String> actions) {
        super(name, coords, adventurerOrientation, actions);
    }

    @Override
    public void move(MapCell[][] mapCells) {
        String  movementType   = actions.get(0);
        Point   newCoordinates = coordinates;
        MapCell currentCell    = mapCells[coordinates.x][coordinates.y];
        Point   maxPoint       = new Point(mapCells.length, mapCells[0].length);
        if (isMovementAStepForward(movementType)) {
            newCoordinates = adventurerOrientation.getNewCoordinates(coordinates, maxPoint);
        } else {
            adventurerOrientation = adventurerOrientation.getNewOrientation(movementType);
        }
        if (!coordinates.equals(newCoordinates)) {
            MapCell nextCell = mapCells[newCoordinates.x][newCoordinates.y];
            if (Boolean.FALSE.equals(nextCell.isMountain()) &&
                Boolean.TRUE.equals(!nextCell.isOccupiedByAnAdventurer())) {
                currentCell.updateAdventurerStatus();
                coordinates = newCoordinates;
                nextCell.updateAdventurerStatus();
                if (nextCell.getTreasures() > 0) {
                    winTreasure(nextCell);
                }
            }
        }
        actions.remove(0);
    }

    private boolean isMovementAStepForward(String movementType) {
        return Objects.equals(movementType, "A");
    }
}