const string input = "Input.txt";

int Part1()
{
    var rope = new Rope();

    foreach (var line in File.ReadLines(input))
    {
        rope.Move(line);
    }

    return rope.TailPositions.Count;
}

int Part2()
{
    var rope = new Rope(10);

    foreach (var line in File.ReadLines(input))
    {
        rope.Move(line);
    }

    return rope.TailPositions.Count;
}

Console.WriteLine(Part1()); // 5695
Console.WriteLine(Part2()); // 2434

public class Rope
{
    private readonly List<Position> _knots = new List<Position>();

    public HashSet<Position> TailPositions { get; } = new HashSet<Position> { new Position() };

    public Rope(int knotCount = 2)
    {
        for (int i = 0; i < knotCount; i++)
        {
            _knots.Add(new Position());
        }
    }

    public Position Head => _knots[0];

    public Position Tail => _knots[^1];

    public void Move(string command)
    {
        var count = int.Parse(command.Substring(2));

        for (int i = 0; i < count; i++)
        {
            switch (command[0])
            {
                case 'U':
                    Head.Y += 1;
                    break;
                case 'D':
                    Head.Y -= 1;
                    break;
                case 'L':
                    Head.X -= 1;
                    break;
                case 'R':
                    Head.X += 1;
                    break;
            }

            MoveTail();
        }
    }

    private void MoveTail()
    {
        for (int i = 1; i < _knots.Count; i++)
        {
            var head = _knots[i - 1];
            var tail = _knots[i];

            if (Math.Abs(head.X - tail.X) < 2 && Math.Abs(head.Y - tail.Y) < 2)
            {
                return;
            }

            tail.X += Math.Sign(head.X - tail.X);
            tail.Y += Math.Sign(head.Y - tail.Y);
        }

        TailPositions.Add(Tail);
    }
}

public record Position
{
    public int X { get; set; }
    public int Y { get; set; }
}
