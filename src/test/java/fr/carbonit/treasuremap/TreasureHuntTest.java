package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.adventurer.AdventurerOrientation;
import fr.carbonit.treasuremap.adventurer.PlainAdventurer;
import fr.carbonit.treasuremap.exception.DataValidity;
import fr.carbonit.treasuremap.treasurehunt.TreasureHunt;
import org.junit.jupiter.api.Test;

import java.awt.*;
import java.util.ArrayDeque;
import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

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
        treasureHunt.putTreasureOnTreasureMap(0, 0, 1);


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
        treasureHunt.putTreasureOnTreasureMap(0, 0, 1);
        treasureHunt.putTreasureOnTreasureMap(0, 0, 1);


        // Then
        assertThat(treasureHunt.getSimulationResult())
                .hasToString(expected
                            );
    }

    @Test
    void should_return_expected_output_after_simulating()
            throws
            DataValidity {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 1 - 0\nM - 2 - 1\nT - 1 - 3 - 2\nA - Lara - 0 - 3 - S - 3";

        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        treasureHunt.putMountainOnTreasureMap(1, 0);
        treasureHunt.putMountainOnTreasureMap(2, 1);
        treasureHunt.putTreasureOnTreasureMap(0, 3, 1);
        treasureHunt.putTreasureOnTreasureMap(0, 3, 1);
        treasureHunt.putTreasureOnTreasureMap(1, 3, 1);
        treasureHunt.putTreasureOnTreasureMap(1, 3, 1);
        treasureHunt.putTreasureOnTreasureMap(1, 3, 1);
        treasureHunt.putAdventurerOnTreasureMap(new PlainAdventurer("Lara",
                                                                    new Point(1, 1),
                                                                    AdventurerOrientation.S,
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

    @Test
    void should_throw_same_location_exception_when_adventurer_on_the_same_position_than_an_other()
            throws
            DataValidity {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        TreasureHunt treasureHunt = new TreasureHunt(mapWidth, mapHeight);
        PlainAdventurer firstAdventurer = new PlainAdventurer("Lara",
                                                              new Point(1, 1),
                                                              AdventurerOrientation.S,
                                                              new ArrayDeque<>(Arrays.asList("A",
                                                                                             "A",
                                                                                             "D",
                                                                                             "A",
                                                                                             "D",
                                                                                             "A",
                                                                                             "G",
                                                                                             "G",
                                                                                             "A")));
        treasureHunt.putAdventurerOnTreasureMap(firstAdventurer);
        // When

        PlainAdventurer secondAdventurer = new PlainAdventurer("Indiana",
                                                               new Point(1, 1),
                                                               AdventurerOrientation.S,
                                                               new ArrayDeque<>(Arrays.asList(
                                                                       "A",
                                                                       "A",
                                                                       "D",
                                                                       "A",
                                                                       "D",
                                                                       "A",
                                                                       "G",
                                                                       "G",
                                                                       "A")));
        assertThatThrownBy(() -> {
            treasureHunt.putAdventurerOnTreasureMap(secondAdventurer);
        }).isInstanceOf(
                DataValidity.class);
    }


}
