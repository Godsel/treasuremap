package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.adventurer.Orientation;
import fr.carbonit.treasuremap.adventurer.PlainAdventurer;
import org.junit.jupiter.api.Test;

import java.awt.*;
import java.util.ArrayDeque;
import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;

class TreasureHuntTest {
    @Test
    void should_return_map_with_width_of_3_cells_and_height_of_4_cells() {
        // Given
        String  expected  = "C - 3 - 4";
        Integer mapWidth  = 3;
        Integer mapHeight = 4;


        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);

        // Then
        assertThat(treasureHunt.getSimulationResult())
                .startsWith(expected);
    }

    @Test
    void should_return_map_with_width_of_4_cells_and_height_of_5_cells() {
        // Given
        Integer mapWidth  = 4;
        Integer mapHeight = 5;

        String expected = "C - 4 - 5";

        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);

        // Then
        assertThat(treasureHunt.getSimulationResult())
                .startsWith(expected);
    }

    @Test
    void should_return_map_with_one_mountain_at_0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        Integer mountainXCoordinate = 0;
        Integer mountainYCoordinate = 0;

        String expected = "C - 3 - 4\nM - 0 - 0";

        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(mountainXCoordinate, mountainYCoordinate);


        // Then
        assertThat(treasureHunt.getSimulationResult())
                .startsWith(expected
                           );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        Integer mountainXCoordinate = 1;
        Integer mountainYCoordinate = 1;

        String expected = "C - 3 - 4\nM - 1 - 1";

        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(mountainXCoordinate, mountainYCoordinate);


        // Then
        assertThat(treasureHunt.getSimulationResult())
                .startsWith(expected
                           );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1_and_one_mountain_at_0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 0 - 0\nM - 1 - 1";

        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(1, 1);
        treasureHunt.putMountainOnTreasureMap(0, 0);


        // Then
        assertThat(treasureHunt.getSimulationResult())
                .startsWith(expected
                           );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1_and_treasure_at0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 1 - 1\nT - 0 - 0 - 1";

        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(1, 1);
        treasureHunt.putTreasureOnTreasureMap(0, 0);


        // Then
        assertThat(treasureHunt.getSimulationResult())
                .hasToString(expected
                            );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1_and_2_treasures_at0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 1 - 1\nT - 0 - 0 - 2";

        // When
        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(1, 1);
        treasureHunt.putTreasureOnTreasureMap(0, 0);
        treasureHunt.putTreasureOnTreasureMap(0, 0);


        // Then
        assertThat(treasureHunt.getSimulationResult())
                .hasToString(expected
                            );
    }

    @Test
    void should_return_expected_output_after_simulating() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 1 - 0\nM - 2 - 1\nT - 1 - 3 - 2\nA - Lara - 0 - 3 - S - 3";

        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(1, 0);
        treasureHunt.putMountainOnTreasureMap(2, 1);
        treasureHunt.putTreasureOnTreasureMap(0, 3);
        treasureHunt.putTreasureOnTreasureMap(0, 3);
        treasureHunt.putTreasureOnTreasureMap(1, 3);
        treasureHunt.putTreasureOnTreasureMap(1, 3);
        treasureHunt.putTreasureOnTreasureMap(1, 3);
        treasureHunt.putAdventurerOnTreasureMap(new PlainAdventurer("Lara",
                                                                    new Point(1, 1),
                                                                    Orientation.S,
                                                                    new ArrayDeque<>(Arrays.asList("A",
                                                                                                   "A",
                                                                                                   "D",
                                                                                                   "A",
                                                                                                   "D",
                                                                                                   "A",
                                                                                                   "G",
                                                                                                   "G",
                                                                                                   "A"))));

        // When
        treasureHunt.simulate();
        String output = treasureHunt.getSimulationResult();

        assertThat(output).isEqualTo(expected);
    }


}
