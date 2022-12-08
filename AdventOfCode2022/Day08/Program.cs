const string input = "Input.txt";

int Part1()
{
    var trees = ParseInput();

    var count = 0;

    for (int i = 0; i < trees.Length; i++)
    {
        for (int j = 0; j < trees[i].Length; j++)
        {
            if (i == 0 || j == 0 || i == trees.Length - 1 || j == trees[i].Length - 1 ||
                Enumerable.Range(0, i).Select(ii => trees[ii][j] < trees[i][j]).All(x => x) ||
                Enumerable.Range(i + 1, trees.Length - i - 1).Select(ii => trees[ii][j] < trees[i][j]).All(x => x) ||
                Enumerable.Range(0, j).Select(jj => trees[i][jj] < trees[i][j]).All(x => x) ||
                Enumerable.Range(j + 1, trees[i].Length - j - 1).Select(jj => trees[i][jj] < trees[i][j]).All(x => x))
            {
                count++;
            }
        }
    }

    return count;
}

int Part2() 
{
    var trees = ParseInput();

    var max = 0;

    for (int i = 0; i < trees.Length; i++)
    {
        for (int j = 0; j < trees[i].Length; j++)
        {
            if (i != 0 && j != 0 && i != trees.Length - 1 && j != trees[i].Length - 1)
            {
                var a = Enumerable.Range(0, i).Reverse().Aggregate((0, true), (result, ii) => result.Item2 ? (result.Item1 + 1, trees[ii][j] < trees[i][j]) : (result.Item1, result.Item2)).Item1;
                var b = Enumerable.Range(i + 1, trees.Length - i - 1).Aggregate((0, true), (result, ii) => result.Item2 ? (result.Item1 + 1, trees[ii][j] < trees[i][j]) : (result.Item1, result.Item2)).Item1;
                var c = Enumerable.Range(0, j).Reverse().Aggregate((0, true), (result, jj) => result.Item2 ? (result.Item1 + 1, trees[i][jj] < trees[i][j]) : (result.Item1, result.Item2)).Item1;
                var d = Enumerable.Range(j + 1, trees[i].Length - j - 1).Aggregate((0, true), (result, jj) => result.Item2 ? (result.Item1 + 1, trees[i][jj] < trees[i][j]) : (result.Item1, result.Item2)).Item1;

                var score = a * b * c * d;

                if (max < score)
                {
                    max = score;
                }
            }
        }
    }

    return max;
}

int[][] ParseInput() => File.ReadLines(input).Select(line => line.Select(x => x - '0').ToArray()).ToArray();

Console.WriteLine(Part1()); // 1705
Console.WriteLine(Part2()); // 371200