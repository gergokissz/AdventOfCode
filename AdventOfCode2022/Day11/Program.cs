long Part1() => new Game(true).Play(20);

long Part2() => new Game(false).Play(10000);

Console.WriteLine(Part1()); // 67830
Console.WriteLine(Part2()); // 15305381442

public class Game
{
    public Game(bool divideByThree)
    {
        _divideByThree = divideByThree;
        _mod = _monkeys.Aggregate(1, (x, y) => x * y.Test);
    }

    // Note: I skipped the parsing today
    private readonly List<Monkey> _monkeys = new List<Monkey>()
    {
        new Monkey(new[] { 56L, 56, 92, 65, 71, 61, 79 }, x => x * 7, 3, 3, 7),
        new Monkey(new[] { 61L, 85 }, x => x + 5, 11, 6, 4),
        new Monkey(new[] { 54L, 96, 82, 78, 69 }, x => x * x, 7, 0, 7),
        new Monkey(new[] { 57L, 59, 65, 95 }, x => x + 4, 2, 5, 1),
        new Monkey(new[] { 62L, 67, 80 }, x => x * 17, 19, 2, 6),
        new Monkey(new[] { 91L }, x => x + 7, 5, 1, 4),
        new Monkey(new[] { 79L, 83, 64, 52, 77, 56, 63, 92 }, x => x + 6, 17, 2, 0),
        new Monkey(new[] { 50L, 97, 76, 96, 80, 56 }, x => x + 3, 13, 3, 5)
    };

    private readonly bool _divideByThree;

    private readonly long _mod;

    public long Play(int rounds)
    {
        for (int i = 0; i < rounds; i++)
        {
            NextRound();
        }

        return CalculateMonkeyBusinessLevel();
    }

    private void NextRound()
    {
        foreach (var monkey in _monkeys)
        {
            while (monkey.TryInspectNextItem(_divideByThree, out var item, out var target))
            {
                _monkeys[target].PassItem(item % _mod);
            }
        }
    }

    private long CalculateMonkeyBusinessLevel()
    {
        var inspectionCounts = _monkeys.Select(x => x.InspectionCount).OrderDescending().ToArray();

        return inspectionCounts[0] * inspectionCounts[1];
    }
}

public record Monkey
{
    public Monkey(IEnumerable<long> items, Func<long, long> operation, int test, int targetIfTrue, int targetIfFalse)
    {
        _items = new Queue<long>(items);
        _operation = operation;
        _targetIfTrue = targetIfTrue;
        _targetIfFalse = targetIfFalse;
        Test = test;
    }

    private Queue<long> _items;

    private Func<long, long> _operation;

    private int _targetIfTrue;

    private int _targetIfFalse;

    public int Test { get; private set; }

    public long InspectionCount { get; private set; }

    public bool TryInspectNextItem(bool divideByThree, out long item, out int target)
    {
        if (!_items.TryDequeue(out item))
        {
            target = 0;

            return false;
        }

        item = _operation(item);

        if (divideByThree)
        {
            item /= 3;
        }

        target = item % Test == 0 ? _targetIfTrue : _targetIfFalse;

        InspectionCount += 1;

        return true;
    }

    public void PassItem(long item) => _items.Enqueue(item);
}