using System.Linq;

const string input = "Input.txt";

long Part1() => ParseDirectoryStructure().GetDirectoriesRecursively().Sum(x => x.Size <= 100000 ? x.Size : 0);

long Part2()
{
    const int storageCapacity = 70000000;
    const int freeSpaceRequired = 30000000;

    var root = ParseDirectoryStructure();

    var freeSpaceNeeded = root.Size - storageCapacity + freeSpaceRequired;

    return ParseDirectoryStructure().GetDirectoriesRecursively().Where(x => x.Size >= freeSpaceNeeded).Min(x => x.Size);
}

Directory ParseDirectoryStructure()
{
    var root = new Directory(null, "/");

    var current = root;

    foreach (var line in System.IO.File.ReadLines(input))
    {
        if (line == "$ cd /")
        {
            current = root;
        }
        else if (line == "$ cd ..")
        {
            if (current.ParentDirectory != null)
            {
                current = current.ParentDirectory;
            }
        }
        else if (line.StartsWith("$ cd "))
        {
            current = current.GetOrdAddDirectory(line[5..]);
        }
        else if (line == "$ ls")
        {
            // ignore
        }
        else if (line.StartsWith("dir "))
        {
            current.GetOrdAddDirectory(line[4..]);
        }
        else
        {
            var index = line.IndexOf(' ');

            current.GetOrAddFile(line[index..], int.Parse(line[0..index]));
        }
    }

    return root;
}

Console.WriteLine(Part1()); // 1334506
Console.WriteLine(Part2()); // 7421137

class Directory
{
    private readonly List<Directory> _directories = new List<Directory>();

    private readonly List<File> _files = new List<File>();

    private readonly Lazy<long> _size;

    public Directory(Directory? parentDirecotry, string name)
    {
        _size = new Lazy<long>(() => _directories.Sum(x => x.Size) + _files.Sum(x => x.Size));

        ParentDirectory = parentDirecotry;
        Name = name;
    }

    public Directory? ParentDirectory { get; }

    public string Name { get; }

    public long Size => _size.Value;

    public Directory GetOrdAddDirectory(string name)
    {
        var ret = _directories.FirstOrDefault(x => x.Name == name);

        if (ret == null)
        {
            ret = new Directory(this, name);

            _directories.Add(ret);
        }

        return ret;
    }

    public File GetOrAddFile(string name, long size)
    {
        var ret = _files.FirstOrDefault(x => x.Name == name);

        if (ret == null)
        {
            ret = new File(this, name, size);

            _files.Add(ret);
        }
        
        return ret;
    }

    public IEnumerable<Directory> GetDirectoriesRecursively()
    {
        yield return this;

        foreach (var directory in _directories)
        {
            foreach (var subDirectory in directory.GetDirectoriesRecursively())
            {
                yield return subDirectory;
            }
        }
    }
}

class File
{
    public File(Directory parentDirectory, string name, long size)
    {
        ParentDirectory = parentDirectory;
        Name = name;
        Size = size;
    }

    public Directory ParentDirectory { get; }

    public string Name { get; }

    public long Size { get; }
}
