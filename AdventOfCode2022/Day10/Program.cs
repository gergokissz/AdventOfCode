const string input = "Input.txt";

int Part1()
{
    var cpu = new Cpu();

    foreach (var line in File.ReadLines(input))
    {
        cpu.Execute(line);
    }

    return cpu.SignalStrength[19] + 
           cpu.SignalStrength[59] +
           cpu.SignalStrength[99] +
           cpu.SignalStrength[139] +
           cpu.SignalStrength[179] +
           cpu.SignalStrength[219];
}

Console.WriteLine(Part1()); // 13680, PZGPKPEB

class Cpu
{
    public int R { get; private set; } = 0;

    public int X { get; private set; } = 1;

    public int Cycle { get; private set; } = 1;

    public List<int> SignalStrength { get; } = new List<int>();

    public void Execute(string command)
    {
        if (command == "noop")
        {
            Tick();
        }
        else if (command.StartsWith("addx "))
        {
            Tick();
            Tick();
            X += int.Parse(command.Substring(5));
        }
    }

    private void Tick()
    {
        SignalStrength.Add(X * Cycle);

        var pixel = (Cycle % 40) - 1;

        if (pixel == X - 1 || pixel == X || pixel == X + 1)
        {
            Console.Write("#");
        }
        else
        { 
            Console.Write(".");
        }

        if (pixel == -1)
        {
            Console.WriteLine();
        }

        Cycle++;
    }
}