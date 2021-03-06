case class AkSim(days: Int = 365, stories: Int = 30, start: Int = 10, opt: Int = 95) {
    implicit class RichInt(n: Int) { def optimal = n min opt; def left = 100 - optimal}
    implicit class RichStr(s: String) { def addIf(cond: Boolean, suff: String) = if (cond) s + suff else s }

    type Times = Int; type Heat = Map[Int, Times]

    def randomly(whatever: Any): Boolean = new java.util.Random nextInt 100 equals 0
    def heat(numbers: Stream[Int]): Heat = numbers groupBy identity mapValues (_.size)
    def beautify(heat: Heat): Heat = {
        val max = heat.values.max; val min = heat.values.min; val diff = (max - min).max(1).toDouble
        heat mapValues (temperature => ((temperature - min) / diff * stories).toInt.max(temperature))
    }

    def mufgAllTheThings(things: Int): Int = things + (1 to things.optimal count randomly min things.left)
    def mufgStory: Stream[Int] = Stream.iterate(start)(mufgAllTheThings) take days
    def mufgStories: Stream[Stream[Int]] = Stream.fill(stories)(mufgStory)
    def mufgMultistories: Stream[Stream[Int]] = mufgStories.transpose
    lazy val mufgStoryHeat: Stream[Heat] = mufgMultistories map heat

    case class Plotter(beauty: Boolean = true, colorbox: Boolean = true, height: Int = 100) {
        def mufgStoryHeat: Stream[Heat] = if (beauty) AkSim.this.mufgStoryHeat map beautify else AkSim.this.mufgStoryHeat

        def data = (for ((hist, day) <- mufgStoryHeat.zipWithIndex; (things, times) <- hist) yield s"$day $things $times").mkString("\n")

        def write() = {
            val writer = new java.io.PrintWriter("graph.gnu")
            val title = s"$stories stories, 1 MUFG, Max $opt Items in MUFG, Start $start".addIf(beauty, ", \"Normalized\"")
            val file = f"S$start%02d-I$stories%06d-N$beauty-O$opt.png"
            writer.println(s"""
                |set term png size 1920,1080 background rgb 'black'; set output '$file'
                |set title '$title' tc rgb 'white'; set arrow from -0.5,95 to $days.5,95 nohead ls 12 front
                |set xrange [-0.5:$days.5]; set yrange [-0.5:$height.5]; unset key ${if (colorbox) "" else "; unset colorbox"}
                |set xlabel 'Days' tc rgb 'white'; set ylabel 'Items' tc rgb 'white'
                |set border lc rgb 'white'; set ytics 0,10; set cbrange [0:$stories]
                |
                |plot '-' using 1:2:3 with points palette
                |$data
                |e
            """.stripMargin)
            writer.close()
        }

        def plot() = { println(AkSim.this + ":" + this); write(); sys.process.Process("gnuplot graph.gnu").! }
    }
}

for {
    start <- Seq(1, 10, 50, 75, 95)
    opt <- Seq(90, 95, 96, 97, 98, 99)
    stories <- Seq(30, 10000)

    days = Map(1 -> 900, 50 -> 200, 75 -> 150, 95 -> 150).getOrElse(start, 365)
    sim = AkSim(start = start, days = days, opt = opt, stories = stories)

    height = Map(1 -> 200, 10 -> 200, 50 -> 250, 75 -> 250, 95 -> 250).getOrElse(start, 100)
    beauty <- Seq(false, true)
} sim.Plotter(colorbox = !beauty, beauty = beauty, height = height).plot()

