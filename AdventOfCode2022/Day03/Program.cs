const string input = "Input.txt";

int Part1() =>
    File.ReadLines(input)
        .Select(SplitInHalf)
        .Sum(x => GetPriority(FindCommonChar(x.Item1, x.Item2, string.Empty)));

int Part2() => 
    File.ReadLines(input)
        .Batch(3)
        .Sum(x => GetPriority(FindCommonChar(x[0], x[1], x[2])));

(string, string) SplitInHalf(string text) =>
    (text[..(text.Length / 2)], text[(text.Length / 2)..text.Length]);

char FindCommonChar(string text1, string text2, string text3)
{
    for (var i = 0; i < text1.Length; i++)
    {
        // Ignore text3 if it is empty
        if (text2.Contains(text1[i]) && (text3.Length == 0 || text3.Contains(text1[i])))
        {
            return text1[i];
        }
    }

    throw new Exception();
}

int GetPriority(char c) => 
    c >= 'a' ? c - 'a' + 1 : c - 'A' + 27;

Console.WriteLine(Part1()); // 8153
Console.WriteLine(Part2()); // 2342

public static class EnumerableExtensions
{
    public static IEnumerable<List<T>> Batch<T>(this IEnumerable<T> items, int batchSize)
    {
        var batch = new List<T>();

        foreach (var item in items)
        {
            batch.Add(item);

            if (batch.Count == batchSize)
            {
                yield return batch;

                batch.Clear();
            }
        }

        if (batch.Count > 0)
        {
            yield return batch;
        }
    }
}
