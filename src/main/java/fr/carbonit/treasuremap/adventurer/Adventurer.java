package fr.carbonit.treasuremap.adventurer;


import fr.carbonit.treasuremap.TreasureHunter;
import fr.carbonit.treasuremap.cell.MapCell;

import java.awt.*;
import java.util.Deque;

public abstract class Adventurer
        implements TreasureHunter {
    private final String        name;
    protected     Integer       numberOfTreasure = 0;
    protected     Point         coordinates;
    protected     Orientation   orientation;
    protected     Deque<String> actions;

    protected Adventurer(String name, Point coords, Orientation orientation, Deque<String> actions) {
        this.name        = name;
        this.coordinates = coords;
        this.orientation = orientation;
        this.actions     = actions;
    }

    public void winTreasure(MapCell mapCell) {
        mapCell.removeTreasure();
        numberOfTreasure++;
    }

    public Point getCoordinates() {
        return coordinates;
    }

    public Boolean hasFinished() {
        return actions.isEmpty();
    }

    public String toString() {
        return "A - " + name + " - " + coordinates.x + " - " + coordinates.y + " - " + orientation.name() + " - " +
               numberOfTreasure;
    }


    public abstract void move(MapCell[][] mapCell);
}

