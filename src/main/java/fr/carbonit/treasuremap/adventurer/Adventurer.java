package fr.carbonit.treasuremap.adventurer;


import fr.carbonit.treasuremap.cell.MapCell;

import java.awt.*;
import java.util.Deque;

public abstract class Adventurer
        implements TreasureHunter {
    private final String                name;
    protected     Integer               numberOfTreasure = 0;
    protected     Point                 coordinates;
    protected     AdventurerOrientation adventurerOrientation;
    protected     Deque<String>         actions;

    protected Adventurer(String name, Point coords, AdventurerOrientation adventurerOrientation,
                         Deque<String> actions) {
        this.name                  = name;
        this.coordinates           = coords;
        this.adventurerOrientation = adventurerOrientation;
        this.actions               = actions;
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
        return "A - " + name + " - " + coordinates.x + " - " + coordinates.y + " - " + adventurerOrientation.name() +
               " - " +
               numberOfTreasure;
    }


    public abstract void move(MapCell[][] mapCell);
}

