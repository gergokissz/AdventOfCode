const string input = "Input.txt";

int Part1() => FindMarker(4);

int Part2() => FindMarker(14);

int FindMarker(int length) 
{
    using var streamReader = new StreamReader(File.OpenRead(input));

    var characters = new Queue<int>(length + 1);
    var character = 0;
    var index = 0;

    while ((character = streamReader.Read()) > -1)
    {
        index++;

        characters.Enqueue(character);

        if (characters.Count == length + 1)
        {
            characters.Dequeue();

            if (characters.Distinct().Count() == characters.Count)
            {
                return index;
            }
        }
    }

    throw new Exception($"Marker with length {length} not found.");
}

Console.WriteLine(Part1()); // 1598
Console.WriteLine(Part2()); // 2414
