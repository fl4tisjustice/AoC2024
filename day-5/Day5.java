import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Day5 {

    private static class ProblemInput {
        public final int[][] rules;
        public final int[][] updates;
        public ProblemInput(int[][] _rules, int[][] _updates) {
            this.rules = _rules;
            this.updates = _updates;
        } 
    }

    public static ProblemInput getInput() {
        try {
            Scanner input = new Scanner(new File("day-5/input.txt"));

            ArrayList<int[]> rules = new ArrayList<int[]>();
            ArrayList<int[]> updates = new ArrayList<int[]>();

            while (input.hasNext("[1-9][0-9]*?\\|[1-9][0-9]*")) {
                String[] line = input.nextLine().split("\\|");
                rules.add(new int[] { Integer.parseInt(line[0]), Integer.parseInt(line[1]) });
            }

            input.nextLine();

            while (input.hasNextLine())
                updates.add(Arrays.stream(input.nextLine().split(",")).<Integer>map(Integer::parseInt).mapToInt(Integer::intValue).toArray());

            input.close();

            return new ProblemInput(rules.toArray(new int[rules.size()][2]), updates.toArray(new int[updates.size()][]));
        } catch (FileNotFoundException e) {
            System.err.println("File not found.");
            System.exit(1);
        }
        return null; 
    }

    public static List<int[]> filterUpdates(final ProblemInput input, boolean valid) {
        final BinaryOperator<Boolean> combinator = (op1, op2) -> valid ? op1 && op2 : op1 || op2;
        
        return Arrays.stream(input.updates)
                     .filter(update -> IntStream.range(0, update.length).boxed().reduce(
                        valid,
                        (pageAccum, pageIdx) -> {
                            boolean updateValidity =
                                Arrays.stream(input.rules).filter(rule -> rule[0] == update[pageIdx]).reduce(
                                    valid,
                                    (accum, rule) -> {
                                        boolean pageValidity = IntStream.of(Arrays.copyOfRange(update, 0, pageIdx)).anyMatch(page -> page == rule[1]);
                                        return valid ? accum && !pageValidity : accum || pageValidity;
                                    },
                                    combinator
                            );
                            return valid ? pageAccum && updateValidity : pageAccum || updateValidity;
                        },
                        combinator))
                     .collect(Collectors.toList());
    }

    public static int partOne(final ProblemInput input) {
        return filterUpdates(input, true).stream()
                    .reduce(0, (accum, update) -> accum + update[update.length / 2], Integer::sum);
    }

    public static int partTwo(final ProblemInput input) {
        return
            filterUpdates(input, false).stream()
                .reduce(0, (accum, update) -> {
                    List<int[]> relevantRules = Arrays.stream(input.rules)
                        .filter(rule -> IntStream.of(update).anyMatch(page -> page == rule[0] && IntStream.of(update).anyMatch(_page -> _page == rule[1])))
                        .collect(Collectors.toList());

                    return accum + relevantRules.stream()
                        .map(rule -> rule[0])
                        .distinct()
                        .sorted((Integer pageOne, Integer pageTwo) ->
                            Long.compare(
                                relevantRules.stream().filter(rule -> rule[0] == pageTwo).count(),
                                relevantRules.stream().filter(rule -> rule[0] == pageOne).count()
                            ))
                        .collect(Collectors.toList()).get(update.length / 2);
                }, Integer::sum);
    }

    public static void main(String[] args) {
        final ProblemInput input = getInput();
        System.out.println(String.format("Part One: %d\nPart Two: %d", partOne(input), partTwo(input)));
    }
}