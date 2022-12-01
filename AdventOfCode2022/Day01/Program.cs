const string input = "Input.txt";

int Part1()
{
    var max = 0;
    var cal = 0;

    foreach (var line in File.ReadLines(input))
    {
        if (string.IsNullOrEmpty(line))
        {
            if (cal > max)
            {
                max = cal;
            }

            cal = 0;

            continue;
        }

        cal += int.Parse(line);
    }

    return max;
}

int Part2()
{
    var queue = new PriorityQueue<int, int>(3);
    var cal = 0;

    foreach (var line in File.ReadLines(input))
    {
        if (string.IsNullOrEmpty(line))
        {
            if (queue.Count < 3)
            {
                queue.Enqueue(cal, cal);
            }
            else if (cal > queue.Peek())
            {
                queue.EnqueueDequeue(cal, cal);
            }

            cal = 0;

            continue;
        }

        cal += int.Parse(line);
    }

    return queue.UnorderedItems.Sum(x => x.Element);
}

Console.WriteLine(Part1());
Console.WriteLine(Part2());
