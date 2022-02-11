package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.adventurer.Adventurer;
import fr.carbonit.treasuremap.adventurer.AdventurerOrientation;
import fr.carbonit.treasuremap.adventurer.PlainAdventurer;
import fr.carbonit.treasuremap.cell.MapCell;
import fr.carbonit.treasuremap.cell.MountainCell;
import fr.carbonit.treasuremap.cell.PlainCell;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class PlainAdventurerTest {

    @Test
    void should_return_Lara_on_0_3_with_south_orientation_and_3_treasures() {
        // Given
        String                expected              = "A - Lara - 0 - 3 - S - 3";
        String                name                  = "Lara";
        AdventurerOrientation adventurerOrientation = AdventurerOrientation.S;
        MapCell[][]           mapCell               = new MapCell[3][4];
        initializeMapWithPlainCells(mapCell);

        // When
        Adventurer plainAdventurer = new PlainAdventurer(name,
                                                         new Point(0, 3),
                                                         adventurerOrientation,
                                                         new ArrayList<>());
        plainAdventurer.winTreasure(new PlainCell(0, 0));
        plainAdventurer.winTreasure(new PlainCell(0, 0));
        plainAdventurer.winTreasure(new PlainCell(0, 0));

        // Then
        Assertions.assertThat(plainAdventurer)
                  .hasToString(expected);
    }

    @Test
    void should_move_to_1_2_when_orientation_is_S_and_action_is_A_and_initial_position_is_1_1() {
        // Given
        String                expected              = "A - Lara - 1 - 2 - S - 0";
        String                name                  = "Lara";
        AdventurerOrientation adventurerOrientation = AdventurerOrientation.S;
        MapCell[][]           mapCell               = new MapCell[3][4];
        initializeMapWithPlainCells(mapCell);

        List<String> actions = new ArrayList<>();
        actions.add("A");
        Adventurer plainAdventurer = new PlainAdventurer(name, new Point(1, 1), adventurerOrientation, actions);
        // When
        plainAdventurer.move(mapCell);

        // Then
        Assertions.assertThat(plainAdventurer)
                  .hasToString(expected);

    }

    @Test
    void should_move_to_1_0_when_orientation_is_N_and_action_is_A_and_initial_position_is_1_1() {
        // Given
        String                expected              = "A - Lara - 1 - 0 - N - 0";
        String                name                  = "Lara";
        AdventurerOrientation adventurerOrientation = AdventurerOrientation.N;
        MapCell[][]           mapCell               = new MapCell[3][4];
        initializeMapWithPlainCells(mapCell);

        List<String> actions = new ArrayList<>();
        actions.add("A");
        Adventurer plainAdventurer = new PlainAdventurer(name, new Point(1, 1), adventurerOrientation, actions);
        // When
        plainAdventurer.move(mapCell);

        // Then
        Assertions.assertThat(plainAdventurer)
                  .hasToString(expected);

    }

    @Test
    void should_have_N_orientation_when_orientation_is_O_and_action_is_D() {
        // Given
        String                expected              = "A - Lara - 1 - 1 - N - 0";
        String                name                  = "Lara";
        AdventurerOrientation adventurerOrientation = AdventurerOrientation.O;
        MapCell[][]           mapCell               = new MapCell[3][4];
        initializeMapWithPlainCells(mapCell);

        List<String> actions = new ArrayList<>();
        actions.add("D");
        Adventurer plainAdventurer = new PlainAdventurer(name,
                                                         new Point(1, 1),
                                                         adventurerOrientation,
                                                         actions);

        //When
        plainAdventurer.move(mapCell);

        // Then
        Assertions.assertThat(plainAdventurer)
                  .hasToString(expected);
    }

    @Test
    void should_have_S_orientation_when_orientation_is_O_and_action_is_G() {
        // Given
        String                expected              = "A - Lara - 1 - 1 - S - 0";
        String                name                  = "Lara";
        AdventurerOrientation adventurerOrientation = AdventurerOrientation.O;
        MapCell[][]           mapCell               = new MapCell[3][4];
        initializeMapWithPlainCells(mapCell);

        List<String> actions = new ArrayList<>();
        actions.add("G");
        Adventurer plainAdventurer = new PlainAdventurer(name,
                                                         new Point(1, 1),
                                                         adventurerOrientation,
                                                         actions);

        //When
        plainAdventurer.move(mapCell);

        // Then
        Assertions.assertThat(plainAdventurer)
                  .hasToString(expected);
    }

    @Test
    void should_return_Lara_0_3_S_0_when_position_is_1_1_and_orientation_S_and_actions_AADADAGGA() {
        // Given
        String                expected              = "A - Lara - 0 - 3 - S - 3";
        String                name                  = "Lara";
        AdventurerOrientation adventurerOrientation = AdventurerOrientation.S;
        MapCell[][]           mapCell               = new MapCell[3][4];
        initializeMapWithPlainCells(mapCell);
        mapCell[1][0] = new MountainCell(1, 0);
        mapCell[2][1] = new MountainCell(2, 1);
        mapCell[0][3].addTreasure();
        mapCell[0][3].addTreasure();
        mapCell[1][3].addTreasure();


        List<String> actions = new ArrayList<>(Arrays.asList("A",
                                                             "A",
                                                             "D",
                                                             "A",
                                                             "D",
                                                             "A",
                                                             "G",
                                                             "G",
                                                             "A"));
        Adventurer plainAdventurer = new PlainAdventurer(name,
                                                         new Point(1, 1),
                                                         adventurerOrientation,
                                                         actions);

        //When
        while (!actions.isEmpty()) {
            plainAdventurer.move(mapCell);
        }

        // Then
        Assertions.assertThat(plainAdventurer)
                  .hasToString(expected);
    }

    private void initializeMapWithPlainCells(MapCell[][] mapCells) {
        for (int mapWidth = 0; mapWidth < mapCells.length; mapWidth++) {
            for (int mapHeight = 0; mapHeight < mapCells[0].length; mapHeight++) {
                mapCells[mapWidth][mapHeight] = new PlainCell(mapWidth, mapHeight);
            }
        }
    }
}
