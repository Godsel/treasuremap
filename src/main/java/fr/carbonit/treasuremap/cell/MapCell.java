package fr.carbonit.treasuremap.cell;

import java.awt.*;

public abstract class MapCell {
    protected Point   coords;
    private   Boolean occupied                = false;
    private   Integer numberOfTreasuresOnCell = 0;

    protected MapCell(Integer xCoordinate, Integer yCoordinate) {
        this.coords = new Point(xCoordinate, yCoordinate);
    }

    public Boolean isMountain() {
        return false;
    }

    public void addTreasure() {
        numberOfTreasuresOnCell++;
    }

    public void removeTreasure() {
        numberOfTreasuresOnCell--;
    }

    public Integer getTreasures() {
        return numberOfTreasuresOnCell;
    }

    public void updateOccupiedStatus() {
        occupied = !occupied;
    }

    public Boolean isOccupied() {
        return occupied;
    }
}
