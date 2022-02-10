package fr.carbonit.treasuremap.adventurer;

import fr.carbonit.treasuremap.cell.MapCell;

import java.awt.*;
import java.util.Deque;
import java.util.Objects;

public class PlainAdventurer
        extends Adventurer {

    public PlainAdventurer(String name, Point coords, Orientation orientation, Deque<String> actions) {
        super(name, coords, orientation, actions);
    }

    @Override
    public void move(MapCell[][] mapCells) {
        String  movementType   = actions.peekFirst();
        Point   newCoordinates = coordinates;
        MapCell currentCell    = mapCells[coordinates.x][coordinates.y];
        Point   maxPoint       = new Point(mapCells.length, mapCells[0].length);
        if (isMovementAStepForward(movementType)) {
            newCoordinates = orientation.getNewCoordinates(coordinates, maxPoint);
        } else {
            orientation = orientation.getNewOrientation(movementType);
        }
        if (!coordinates.equals(newCoordinates)) {
            MapCell nextCell = mapCells[newCoordinates.x][newCoordinates.y];
            if (Boolean.FALSE.equals(nextCell.isMountain()) && Boolean.TRUE.equals(!nextCell.isOccupied())) {
                currentCell.updateOccupiedStatus();
                coordinates = newCoordinates;
                nextCell.updateOccupiedStatus();
                if (nextCell.getTreasures() > 0) {
                    winTreasure(nextCell);
                }
            }
        }
        actions.pollFirst();
    }

    private boolean isMovementAStepForward(String movementType) {
        return Objects.equals(movementType, "A");
    }
}