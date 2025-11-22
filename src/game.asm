# MARK: Constants

# framebuffer
.eqv FRAMEBUFFER_WIDTH 256 # in pixels
.eqv FRAMEBUFFER_HEIGHT 256 # in pixels
.eqv FRAMEBUFFER_STRIDE 1024 # (FRAMEBUFFER_WIDTH * 4) in bytes
.eqv FRAMEBUFFER_STRIDE_SHIFT 10 # 2^(x) = FRAMEBUFFER_STRIDE
.eqv FRAMEBUFFER_SIZE 0x40000 # (FRAMEBUFFER_STRIDE * FRAMEBUFFER_HEIGHT) in bytes

# game logic
.eqv DICTIONARY_LEN 2315

# user interface
.eqv TILE_SIZE 28
.eqv TILE_INNER_SIZE 24

.eqv COLOR_WHITE 0x00ffffff
.eqv COLOR_BLACK 0x00000000
.eqv COLOR_LIGHT_GRAY 0x00d3d6da
.eqv COLOR_GRAY: 0x00787c7e
.eqv COLOR_DARK_GRAY 0x00878a8c
.eqv COLOR_YELLOW 0x00c9b458
.eqv COLOR_GREEN 0x0067a561

# MARK: Data

.data
framebuffer: .space FRAMEBUFFER_SIZE

dictionary: .ascii "aback" "abase" "abate" "abbey" "abbot" "abhor" "abide" "abled" "abode" "abort" "about" "above" "abuse" "abyss" "acorn" "acrid" "actor" "acute" "adage" "adapt" "adept" "admin" "admit" "adobe" "adopt" "adore" "adorn" "adult" "affix" "afire" "afoot" "afoul" "after" "again" "agape" "agate" "agent" "agile" "aging" "aglow" "agony" "agora" "agree" "ahead" "aider" "aisle" "alarm" "album" "alert" "algae" "alibi" "alien" "align" "alike" "alive" "allay" "alley" "allot" "allow" "alloy" "aloft" "alone" "along" "aloof" "aloud" "alpha" "altar" "alter" "amass" "amaze" "amber" "amble" "amend" "amiss" "amity" "among" "ample" "amply" "amuse" "angel" "anger" "angle" "angry" "angst" "anime" "ankle" "annex" "annoy" "annul" "anode" "antic" "anvil" "aorta" "apart" "aphid" "aping" "apnea" "apple" "apply" "apron" "aptly" "arbor" "ardor" "arena" "argue" "arise" "armor" "aroma" "arose" "array" "arrow" "arson" "artsy" "ascot" "ashen" "aside" "askew" "assay" "asset" "atoll" "atone" "attic" "audio" "audit" "augur" "aunty" "avail" "avert" "avian" "avoid" "await" "awake" "award" "aware" "awash" "awful" "awoke" "axial" "axiom" "axion" "azure" "bacon" "badge" "badly" "bagel" "baggy" "baker" "baler" "balmy" "banal" "banjo" "barge" "baron" "basal" "basic" "basil" "basin" "basis" "baste" "batch" "bathe" "baton" "batty" "bawdy" "bayou" "beach" "beady" "beard" "beast" "beech" "beefy" "befit" "began" "begat" "beget" "begin" "begun" "being" "belch" "belie" "belle" "belly" "below" "bench" "beret" "berry" "berth" "beset" "betel" "bevel" "bezel" "bible" "bicep" "biddy" "bigot" "bilge" "billy" "binge" "bingo" "biome" "birch" "birth" "bison" "bitty" "black" "blade" "blame" "bland" "blank" "blare" "blast" "blaze" "bleak" "bleat" "bleed" "bleep" "blend" "bless" "blimp" "blind" "blink" "bliss" "blitz" "bloat" "block" "bloke" "blond" "blood" "bloom" "blown" "bluer" "bluff" "blunt" "blurb" "blurt" "blush" "board" "boast" "bobby" "boney" "bongo" "bonus" "booby" "boost" "booth" "booty" "booze" "boozy" "borax" "borne" "bosom" "bossy" "botch" "bough" "boule" "bound" "bowel" "boxer" "brace" "braid" "brain" "brake" "brand" "brash" "brass" "brave" "bravo" "brawl" "brawn" "bread" "break" "breed" "briar" "bribe" "brick" "bride" "brief" "brine" "bring" "brink" "briny" "brisk" "broad" "broil" "broke" "brood" "brook" "broom" "broth" "brown" "brunt" "brush" "brute" "buddy" "budge" "buggy" "bugle" "build" "built" "bulge" "bulky" "bully" "bunch" "bunny" "burly" "burnt" "burst" "bused" "bushy" "butch" "butte" "buxom" "buyer" "bylaw" "cabal" "cabby" "cabin" "cable" "cacao" "cache" "cacti" "caddy" "cadet" "cagey" "cairn" "camel" "cameo" "canal" "candy" "canny" "canoe" "canon" "caper" "caput" "carat" "cargo" "carol" "carry" "carve" "caste" "catch" "cater" "catty" "caulk" "cause" "cavil" "cease" "cedar" "cello" "chafe" "chaff" "chain" "chair" "chalk" "champ" "chant" "chaos" "chard" "charm" "chart" "chase" "chasm" "cheap" "cheat" "check" "cheek" "cheer" "chess" "chest" "chick" "chide" "chief" "child" "chili" "chill" "chime" "china" "chirp" "chock" "choir" "choke" "chord" "chore" "chose" "chuck" "chump" "chunk" "churn" "chute" "cider" "cigar" "cinch" "circa" "civic" "civil" "clack" "claim" "clamp" "clang" "clank" "clash" "clasp" "class" "clean" "clear" "cleat" "cleft" "clerk" "click" "cliff" "climb" "cling" "clink" "cloak" "clock" "clone" "close" "cloth" "cloud" "clout" "clove" "clown" "cluck" "clued" "clump" "clung" "coach" "coast" "cobra" "cocoa" "colon" "color" "comet" "comfy" "comic" "comma" "conch" "condo" "conic" "copse" "coral" "corer" "corny" "couch" "cough" "could" "count" "coupe" "court" "coven" "cover" "covet" "covey" "cower" "coyly" "crack" "craft" "cramp" "crane" "crank" "crash" "crass" "crate" "crave" "crawl" "craze" "crazy" "creak" "cream" "credo" "creed" "creek" "creep" "creme" "crepe" "crept" "cress" "crest" "crick" "cried" "crier" "crime" "crimp" "crisp" "croak" "crock" "crone" "crony" "crook" "cross" "croup" "crowd" "crown" "crude" "cruel" "crumb" "crump" "crush" "crust" "crypt" "cubic" "cumin" "curio" "curly" "curry" "curse" "curve" "curvy" "cutie" "cyber" "cycle" "cynic" "daddy" "daily" "dairy" "daisy" "dally" "dance" "dandy" "datum" "daunt" "dealt" "death" "debar" "debit" "debug" "debut" "decal" "decay" "decor" "decoy" "decry" "defer" "deign" "deity" "delay" "delta" "delve" "demon" "demur" "denim" "dense" "depot" "depth" "derby" "deter" "detox" "deuce" "devil" "diary" "dicey" "digit" "dilly" "dimly" "diner" "dingo" "dingy" "diode" "dirge" "dirty" "disco" "ditch" "ditto" "ditty" "diver" "dizzy" "dodge" "dodgy" "dogma" "doing" "dolly" "donor" "donut" "dopey" "doubt" "dough" "dowdy" "dowel" "downy" "dowry" "dozen" "draft" "drain" "drake" "drama" "drank" "drape" "drawl" "drawn" "dread" "dream" "dress" "dried" "drier" "drift" "drill" "drink" "drive" "droit" "droll" "drone" "drool" "droop" "dross" "drove" "drown" "druid" "drunk" "dryer" "dryly" "duchy" "dully" "dummy" "dumpy" "dunce" "dusky" "dusty" "dutch" "duvet" "dwarf" "dwell" "dwelt" "dying" "eager" "eagle" "early" "earth" "easel" "eaten" "eater" "ebony" "eclat" "edict" "edify" "eerie" "egret" "eight" "eject" "eking" "elate" "elbow" "elder" "elect" "elegy" "elfin" "elide" "elite" "elope" "elude" "email" "embed" "ember" "emcee" "empty" "enact" "endow" "enema" "enemy" "enjoy" "ennui" "ensue" "enter" "entry" "envoy" "epoch" "epoxy" "equal" "equip" "erase" "erect" "erode" "error" "erupt" "essay" "ester" "ether" "ethic" "ethos" "etude" "evade" "event" "every" "evict" "evoke" "exact" "exalt" "excel" "exert" "exile" "exist" "expel" "extol" "extra" "exult" "eying" "fable" "facet" "faint" "fairy" "faith" "false" "fancy" "fanny" "farce" "fatal" "fatty" "fault" "fauna" "favor" "feast" "fecal" "feign" "fella" "felon" "femme" "femur" "fence" "feral" "ferry" "fetal" "fetch" "fetid" "fetus" "fever" "fewer" "fiber" "fibre" "ficus" "field" "fiend" "fiery" "fifth" "fifty" "fight" "filer" "filet" "filly" "filmy" "filth" "final" "finch" "finer" "first" "fishy" "fixer" "fizzy" "fjord" "flack" "flail" "flair" "flake" "flaky" "flame" "flank" "flare" "flash" "flask" "fleck" "fleet" "flesh" "flick" "flier" "fling" "flint" "flirt" "float" "flock" "flood" "floor" "flora" "floss" "flour" "flout" "flown" "fluff" "fluid" "fluke" "flume" "flung" "flunk" "flush" "flute" "flyer" "foamy" "focal" "focus" "foggy" "foist" "folio" "folly" "foray" "force" "forge" "forgo" "forte" "forth" "forty" "forum" "found" "foyer" "frail" "frame" "frank" "fraud" "freak" "freed" "freer" "fresh" "friar" "fried" "frill" "frisk" "fritz" "frock" "frond" "front" "frost" "froth" "frown" "froze" "fruit" "fudge" "fugue" "fully" "fungi" "funky" "funny" "furor" "furry" "fussy" "fuzzy" "gaffe" "gaily" "gamer" "gamma" "gamut" "gassy" "gaudy" "gauge" "gaunt" "gauze" "gavel" "gawky" "gayer" "gayly" "gazer" "gecko" "geeky" "geese" "genie" "genre" "ghost" "ghoul" "giant" "giddy" "gipsy" "girly" "girth" "given" "giver" "glade" "gland" "glare" "glass" "glaze" "gleam" "glean" "glide" "glint" "gloat" "globe" "gloom" "glory" "gloss" "glove" "glyph" "gnash" "gnome" "godly" "going" "golem" "golly" "gonad" "goner" "goody" "gooey" "goofy" "goose" "gorge" "gouge" "gourd" "grace" "grade" "graft" "grail" "grain" "grand" "grant" "grape" "graph" "grasp" "grass" "grate" "grave" "gravy" "graze" "great" "greed" "green" "greet" "grief" "grill" "grime" "grimy" "grind" "gripe" "groan" "groin" "groom" "grope" "gross" "group" "grout" "grove" "growl" "grown" "gruel" "gruff" "grunt" "guard" "guava" "guess" "guest" "guide" "guild" "guile" "guilt" "guise" "gulch" "gully" "gumbo" "gummy" "guppy" "gusto" "gusty" "gypsy" "habit" "hairy" "halve" "handy" "happy" "hardy" "harem" "harpy" "harry" "harsh" "haste" "hasty" "hatch" "hater" "haunt" "haute" "haven" "havoc" "hazel" "heady" "heard" "heart" "heath" "heave" "heavy" "hedge" "hefty" "heist" "helix" "hello" "hence" "heron" "hilly" "hinge" "hippo" "hippy" "hitch" "hoard" "hobby" "hoist" "holly" "homer" "honey" "honor" "horde" "horny" "horse" "hotel" "hotly" "hound" "house" "hovel" "hover" "howdy" "human" "humid" "humor" "humph" "humus" "hunch" "hunky" "hurry" "husky" "hussy" "hutch" "hydro" "hyena" "hymen" "hyper" "icily" "icing" "ideal" "idiom" "idiot" "idler" "idyll" "igloo" "iliac" "image" "imbue" "impel" "imply" "inane" "inbox" "incur" "index" "inept" "inert" "infer" "ingot" "inlay" "inlet" "inner" "input" "inter" "intro" "ionic" "irate" "irony" "islet" "issue" "itchy" "ivory" "jaunt" "jazzy" "jelly" "jerky" "jetty" "jewel" "jiffy" "joint" "joist" "joker" "jolly" "joust" "judge" "juice" "juicy" "jumbo" "jumpy" "junta" "junto" "juror" "kappa" "karma" "kayak" "kebab" "khaki" "kinky" "kiosk" "kitty" "knack" "knave" "knead" "kneed" "kneel" "knelt" "knife" "knock" "knoll" "known" "koala" "krill" "label" "labor" "laden" "ladle" "lager" "lance" "lanky" "lapel" "lapse" "large" "larva" "lasso" "latch" "later" "lathe" "latte" "laugh" "layer" "leach" "leafy" "leaky" "leant" "leapt" "learn" "lease" "leash" "least" "leave" "ledge" "leech" "leery" "lefty" "legal" "leggy" "lemon" "lemur" "leper" "level" "lever" "libel" "liege" "light" "liken" "lilac" "limbo" "limit" "linen" "liner" "lingo" "lipid" "lithe" "liver" "livid" "llama" "loamy" "loath" "lobby" "local" "locus" "lodge" "lofty" "logic" "login" "loopy" "loose" "lorry" "loser" "louse" "lousy" "lover" "lower" "lowly" "loyal" "lucid" "lucky" "lumen" "lumpy" "lunar" "lunch" "lunge" "lupus" "lurch" "lurid" "lusty" "lying" "lymph" "lynch" "lyric" "macaw" "macho" "macro" "madam" "madly" "mafia" "magic" "magma" "maize" "major" "maker" "mambo" "mamma" "mammy" "manga" "mange" "mango" "mangy" "mania" "manic" "manly" "manor" "maple" "march" "marry" "marsh" "mason" "masse" "match" "matey" "mauve" "maxim" "maybe" "mayor" "mealy" "meant" "meaty" "mecca" "medal" "media" "medic" "melee" "melon" "mercy" "merge" "merit" "merry" "metal" "meter" "metro" "micro" "midge" "midst" "might" "milky" "mimic" "mince" "miner" "minim" "minor" "minty" "minus" "mirth" "miser" "missy" "mocha" "modal" "model" "modem" "mogul" "moist" "molar" "moldy" "money" "month" "moody" "moose" "moral" "moron" "morph" "mossy" "motel" "motif" "motor" "motto" "moult" "mound" "mount" "mourn" "mouse" "mouth" "mover" "movie" "mower" "mucky" "mucus" "muddy" "mulch" "mummy" "munch" "mural" "murky" "mushy" "music" "musky" "musty" "myrrh" "nadir" "naive" "nanny" "nasal" "nasty" "natal" "naval" "navel" "needy" "neigh" "nerdy" "nerve" "never" "newer" "newly" "nicer" "niche" "niece" "night" "ninja" "ninny" "ninth" "noble" "nobly" "noise" "noisy" "nomad" "noose" "north" "nosey" "notch" "novel" "nudge" "nurse" "nutty" "nylon" "nymph" "oaken" "obese" "occur" "ocean" "octal" "octet" "odder" "oddly" "offal" "offer" "often" "olden" "older" "olive" "ombre" "omega" "onion" "onset" "opera" "opine" "opium" "optic" "orbit" "order" "organ" "other" "otter" "ought" "ounce" "outdo" "outer" "outgo" "ovary" "ovate" "overt" "ovine" "ovoid" "owing" "owner" "oxide" "ozone" "paddy" "pagan" "paint" "paler" "palsy" "panel" "panic" "pansy" "papal" "paper" "parer" "parka" "parry" "parse" "party" "pasta" "paste" "pasty" "patch" "patio" "patsy" "patty" "pause" "payee" "payer" "peace" "peach" "pearl" "pecan" "pedal" "penal" "pence" "penne" "penny" "perch" "peril" "perky" "pesky" "pesto" "petal" "petty" "phase" "phone" "phony" "photo" "piano" "picky" "piece" "piety" "piggy" "pilot" "pinch" "piney" "pinky" "pinto" "piper" "pique" "pitch" "pithy" "pivot" "pixel" "pixie" "pizza" "place" "plaid" "plain" "plait" "plane" "plank" "plant" "plate" "plaza" "plead" "pleat" "plied" "plier" "pluck" "plumb" "plume" "plump" "plunk" "plush" "poesy" "point" "poise" "poker" "polar" "polka" "polyp" "pooch" "poppy" "porch" "poser" "posit" "posse" "pouch" "pound" "pouty" "power" "prank" "prawn" "preen" "press" "price" "prick" "pride" "pried" "prime" "primo" "print" "prior" "prism" "privy" "prize" "probe" "prone" "prong" "proof" "prose" "proud" "prove" "prowl" "proxy" "prude" "prune" "psalm" "pubic" "pudgy" "puffy" "pulpy" "pulse" "punch" "pupal" "pupil" "puppy" "puree" "purer" "purge" "purse" "pushy" "putty" "pygmy" "quack" "quail" "quake" "qualm" "quark" "quart" "quash" "quasi" "queen" "queer" "quell" "query" "quest" "queue" "quick" "quiet" "quill" "quilt" "quirk" "quite" "quota" "quote" "quoth" "rabbi" "rabid" "racer" "radar" "radii" "radio" "rainy" "raise" "rajah" "rally" "ralph" "ramen" "ranch" "randy" "range" "rapid" "rarer" "raspy" "ratio" "ratty" "raven" "rayon" "razor" "reach" "react" "ready" "realm" "rearm" "rebar" "rebel" "rebus" "rebut" "recap" "recur" "recut" "reedy" "refer" "refit" "regal" "rehab" "reign" "relax" "relay" "relic" "remit" "renal" "renew" "repay" "repel" "reply" "rerun" "reset" "resin" "retch" "retro" "retry" "reuse" "revel" "revue" "rhino" "rhyme" "rider" "ridge" "rifle" "right" "rigid" "rigor" "rinse" "ripen" "riper" "risen" "riser" "risky" "rival" "river" "rivet" "roach" "roast" "robin" "robot" "rocky" "rodeo" "roger" "rogue" "roomy" "roost" "rotor" "rouge" "rough" "round" "rouse" "route" "rover" "rowdy" "rower" "royal" "ruddy" "ruder" "rugby" "ruler" "rumba" "rumor" "rupee" "rural" "rusty" "sadly" "safer" "saint" "salad" "sally" "salon" "salsa" "salty" "salve" "salvo" "sandy" "saner" "sappy" "sassy" "satin" "satyr" "sauce" "saucy" "sauna" "saute" "savor" "savoy" "savvy" "scald" "scale" "scalp" "scaly" "scamp" "scant" "scare" "scarf" "scary" "scene" "scent" "scion" "scoff" "scold" "scone" "scoop" "scope" "score" "scorn" "scour" "scout" "scowl" "scram" "scrap" "scree" "screw" "scrub" "scrum" "scuba" "sedan" "seedy" "segue" "seize" "semen" "sense" "sepia" "serif" "serum" "serve" "setup" "seven" "sever" "sewer" "shack" "shade" "shady" "shaft" "shake" "shaky" "shale" "shall" "shalt" "shame" "shank" "shape" "shard" "share" "shark" "sharp" "shave" "shawl" "shear" "sheen" "sheep" "sheer" "sheet" "sheik" "shelf" "shell" "shied" "shift" "shine" "shiny" "shire" "shirk" "shirt" "shoal" "shock" "shone" "shook" "shoot" "shore" "shorn" "short" "shout" "shove" "shown" "showy" "shrew" "shrub" "shrug" "shuck" "shunt" "shush" "shyly" "siege" "sieve" "sight" "sigma" "silky" "silly" "since" "sinew" "singe" "siren" "sissy" "sixth" "sixty" "skate" "skier" "skiff" "skill" "skimp" "skirt" "skulk" "skull" "skunk" "slack" "slain" "slang" "slant" "slash" "slate" "slave" "sleek" "sleep" "sleet" "slept" "slice" "slick" "slide" "slime" "slimy" "sling" "slink" "sloop" "slope" "slosh" "sloth" "slump" "slung" "slunk" "slurp" "slush" "slyly" "smack" "small" "smart" "smash" "smear" "smell" "smelt" "smile" "smirk" "smite" "smith" "smock" "smoke" "smoky" "smote" "snack" "snail" "snake" "snaky" "snare" "snarl" "sneak" "sneer" "snide" "sniff" "snipe" "snoop" "snore" "snort" "snout" "snowy" "snuck" "snuff" "soapy" "sober" "soggy" "solar" "solid" "solve" "sonar" "sonic" "sooth" "sooty" "sorry" "sound" "south" "sower" "space" "spade" "spank" "spare" "spark" "spasm" "spawn" "speak" "spear" "speck" "speed" "spell" "spelt" "spend" "spent" "sperm" "spice" "spicy" "spied" "spiel" "spike" "spiky" "spill" "spilt" "spine" "spiny" "spire" "spite" "splat" "split" "spoil" "spoke" "spoof" "spook" "spool" "spoon" "spore" "sport" "spout" "spray" "spree" "sprig" "spunk" "spurn" "spurt" "squad" "squat" "squib" "stack" "staff" "stage" "staid" "stain" "stair" "stake" "stale" "stalk" "stall" "stamp" "stand" "stank" "stare" "stark" "start" "stash" "state" "stave" "stead" "steak" "steal" "steam" "steed" "steel" "steep" "steer" "stein" "stern" "stick" "stiff" "still" "stilt" "sting" "stink" "stint" "stock" "stoic" "stoke" "stole" "stomp" "stone" "stony" "stood" "stool" "stoop" "store" "stork" "storm" "story" "stout" "stove" "strap" "straw" "stray" "strip" "strut" "stuck" "study" "stuff" "stump" "stung" "stunk" "stunt" "style" "suave" "sugar" "suing" "suite" "sulky" "sully" "sumac" "sunny" "super" "surer" "surge" "surly" "sushi" "swami" "swamp" "swarm" "swash" "swath" "swear" "sweat" "sweep" "sweet" "swell" "swept" "swift" "swill" "swine" "swing" "swirl" "swish" "swoon" "swoop" "sword" "swore" "sworn" "swung" "synod" "syrup" "tabby" "table" "taboo" "tacit" "tacky" "taffy" "taint" "taken" "taker" "tally" "talon" "tamer" "tango" "tangy" "taper" "tapir" "tardy" "tarot" "taste" "tasty" "tatty" "taunt" "tawny" "teach" "teary" "tease" "teddy" "teeth" "tempo" "tenet" "tenor" "tense" "tenth" "tepee" "tepid" "terra" "terse" "testy" "thank" "theft" "their" "theme" "there" "these" "theta" "thick" "thief" "thigh" "thing" "think" "third" "thong" "thorn" "those" "three" "threw" "throb" "throw" "thrum" "thumb" "thump" "thyme" "tiara" "tibia" "tidal" "tiger" "tight" "tilde" "timer" "timid" "tipsy" "titan" "tithe" "title" "toast" "today" "toddy" "token" "tonal" "tonga" "tonic" "tooth" "topaz" "topic" "torch" "torso" "torus" "total" "totem" "touch" "tough" "towel" "tower" "toxic" "toxin" "trace" "track" "tract" "trade" "trail" "train" "trait" "tramp" "trash" "trawl" "tread" "treat" "trend" "triad" "trial" "tribe" "trice" "trick" "tried" "tripe" "trite" "troll" "troop" "trope" "trout" "trove" "truce" "truck" "truer" "truly" "trump" "trunk" "truss" "trust" "truth" "tryst" "tubal" "tuber" "tulip" "tulle" "tumor" "tunic" "turbo" "tutor" "twang" "tweak" "tweed" "tweet" "twice" "twine" "twirl" "twist" "twixt" "tying" "udder" "ulcer" "ultra" "umbra" "uncle" "uncut" "under" "undid" "undue" "unfed" "unfit" "unify" "union" "unite" "unity" "unlit" "unmet" "unset" "untie" "until" "unwed" "unzip" "upper" "upset" "urban" "urine" "usage" "usher" "using" "usual" "usurp" "utile" "utter" "vague" "valet" "valid" "valor" "value" "valve" "vapid" "vapor" "vault" "vaunt" "vegan" "venom" "venue" "verge" "verse" "verso" "verve" "vicar" "video" "vigil" "vigor" "villa" "vinyl" "viola" "viper" "viral" "virus" "visit" "visor" "vista" "vital" "vivid" "vixen" "vocal" "vodka" "vogue" "voice" "voila" "vomit" "voter" "vouch" "vowel" "vying" "wacky" "wafer" "wager" "wagon" "waist" "waive" "waltz" "warty" "waste" "watch" "water" "waver" "waxen" "weary" "weave" "wedge" "weedy" "weigh" "weird" "welch" "welsh" "wench" "whack" "whale" "wharf" "wheat" "wheel" "whelp" "where" "which" "whiff" "while" "whine" "whiny" "whirl" "whisk" "white" "whole" "whoop" "whose" "widen" "wider" "widow" "width" "wield" "wight" "willy" "wimpy" "wince" "winch" "windy" "wiser" "wispy" "witch" "witty" "woken" "woman" "women" "woody" "wooer" "wooly" "woozy" "wordy" "world" "worry" "worse" "worst" "worth" "would" "wound" "woven" "wrack" "wrath" "wreak" "wreck" "wrest" "wring" "wrist" "write" "wrong" "wrote" "wrung" "wryly" "yacht" "yearn" "yeast" "yield" "young" "youth" "zebra" "zesty" "zonal"

font:
    char_exclam: .word 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_quotedbl: .word 0x0000 0x0000 0x1980 0x1980 0x1980 0x1980 0x1980 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_numbersign: .word 0x0000 0x0000 0x0000 0x0000 0x1980 0x1980 0x1980 0x1980 0x7FE0 0x1980 0x1980 0x1980 0x1980 0x1980 0x7FE0 0x1980 0x1980 0x1980 0x1980 0x0000 0x0000 0x0000 0x0000 0x0000
    char_dollar: .word 0x0000 0x0000 0x0000 0x0600 0x0600 0x1F80 0x36C0 0x6660 0x6600 0x6600 0x3600 0x1F80 0x06C0 0x0660 0x0660 0x6660 0x36C0 0x1F80 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000
    char_percent: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x38C0 0x6CC0 0x6D80 0x3980 0x0300 0x0300 0x0600 0x0600 0x0C00 0x0C00 0x19C0 0x1B60 0x3360 0x31C0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_ampersand: .word 0x0000 0x0000 0x0000 0x0000 0x0E00 0x1B00 0x3180 0x3180 0x3180 0x1B00 0x0E00 0x1E60 0x3360 0x61C0 0x60C0 0x60C0 0x61C0 0x3360 0x1E60 0x0000 0x0000 0x0000 0x0000 0x0000
    char_quotesingle: .word 0x0000 0x0000 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_parenleft: .word 0x0000 0x0000 0x0000 0x0000 0x0300 0x0600 0x0C00 0x0C00 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x0C00 0x0C00 0x0600 0x0300 0x0000 0x0000 0x0000 0x0000 0x0000
    char_parenright: .word 0x0000 0x0000 0x0000 0x0000 0x1800 0x0C00 0x0600 0x0600 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0600 0x0600 0x0C00 0x1800 0x0000 0x0000 0x0000 0x0000 0x0000
    char_asterisk: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x60C0 0x3180 0x1B00 0x0E00 0xFFE0 0x0E00 0x1B00 0x3180 0x60C0 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_plus: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0600 0x7FE0 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_comma: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0600 0x0C00 0x0000 0x0000 0x0000 0x0000
    char_hyphen: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_period: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_slash: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x00C0 0x00C0 0x0180 0x0180 0x0300 0x0300 0x0600 0x0600 0x0C00 0x0C00 0x1800 0x1800 0x3000 0x3000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_zero: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x60E0 0x61E0 0x6360 0x6660 0x6C60 0x7860 0x7060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_one: .word 0x0000 0x0000 0x0000 0x0000 0x0600 0x0E00 0x1E00 0x3600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x3FC0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_two: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x0060 0x00C0 0x0180 0x0300 0x0600 0x0C00 0x1800 0x3000 0x6000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_three: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x0060 0x0060 0x0060 0x00C0 0x0F80 0x00C0 0x0060 0x0060 0x0060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_four: .word 0x0000 0x0000 0x0000 0x0000 0x0060 0x00E0 0x01E0 0x0360 0x0660 0x0C60 0x1860 0x3060 0x6060 0x6060 0x6060 0x7FE0 0x0060 0x0060 0x0060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_five: .word 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x6000 0x6000 0x6000 0x6000 0x6000 0x7F80 0x00C0 0x0060 0x0060 0x0060 0x0060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_six: .word 0x0000 0x0000 0x0000 0x0000 0x1FC0 0x3000 0x6000 0x6000 0x6000 0x6000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_seven: .word 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x6060 0x6060 0x0060 0x00C0 0x00C0 0x0180 0x0180 0x0300 0x0300 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_eight: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_nine: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x3060 0x1FE0 0x0060 0x0060 0x0060 0x0060 0x00C0 0x3F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_colon: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_semicolon: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0600 0x0C00 0x0000 0x0000 0x0000 0x0000
    char_less: .word 0x0000 0x0000 0x0000 0x0000 0x00C0 0x0180 0x0300 0x0600 0x0C00 0x1800 0x3000 0x6000 0x3000 0x1800 0x0C00 0x0600 0x0300 0x0180 0x00C0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_equal: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_greater: .word 0x0000 0x0000 0x0000 0x0000 0x6000 0x3000 0x1800 0x0C00 0x0600 0x0300 0x0180 0x00C0 0x0180 0x0300 0x0600 0x0C00 0x1800 0x3000 0x6000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_question: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x00C0 0x0180 0x0300 0x0600 0x0600 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_at: .word 0x0000 0x0000 0x0000 0x0000 0x3F80 0x60C0 0xC060 0xC3E0 0xC660 0xCC60 0xCC60 0xCC60 0xCC60 0xCC60 0xC660 0xC3E0 0xC000 0x6000 0x3FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_A: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x7FE0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_B: .word 0x0000 0x0000 0x0000 0x0000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_C: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_D: .word 0x0000 0x0000 0x0000 0x0000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_E: .word 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x7F80 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_F: .word 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x7F80 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_G: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6000 0x6000 0x6000 0x63E0 0x6060 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_H: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x7FE0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_I: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_J: .word 0x0000 0x0000 0x0000 0x0000 0x03F0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x60C0 0x60C0 0x60C0 0x3180 0x1F00 0x0000 0x0000 0x0000 0x0000 0x0000
    char_K: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x60C0 0x6180 0x6300 0x6600 0x6C00 0x7800 0x7000 0x7800 0x6C00 0x6600 0x6300 0x6180 0x60C0 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_L: .word 0x0000 0x0000 0x0000 0x0000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_M: .word 0x0000 0x0000 0x0000 0x0000 0x8020 0xC060 0xE0E0 0xF1E0 0xDB60 0xCE60 0xC460 0xC060 0xC060 0xC060 0xC060 0xC060 0xC060 0xC060 0xC060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_N: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x7060 0x7860 0x6C60 0x6660 0x6360 0x61E0 0x60E0 0x6060 0x6060 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_O: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_P: .word 0x0000 0x0000 0x0000 0x0000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_Q: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6660 0x33C0 0x1F80 0x00C0 0x0060 0x0000 0x0000 0x0000
    char_R: .word 0x0000 0x0000 0x0000 0x0000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x7800 0x6C00 0x6600 0x6300 0x6180 0x60C0 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_S: .word 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6000 0x6000 0x6000 0x3000 0x1F80 0x00C0 0x0060 0x0060 0x0060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_T: .word 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_U: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_V: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x30C0 0x30C0 0x30C0 0x30C0 0x1980 0x1980 0x1980 0x0F00 0x0F00 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_W: .word 0x0000 0x0000 0x0000 0x0000 0xC060 0xC060 0xC060 0xC060 0xC060 0xC060 0xC060 0xC060 0xC460 0xCE60 0xDB60 0xF1E0 0xE0E0 0xC060 0x8020 0x0000 0x0000 0x0000 0x0000 0x0000
    char_X: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x30C0 0x30C0 0x1980 0x1980 0x0F00 0x0600 0x0F00 0x1980 0x1980 0x30C0 0x30C0 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_Y: .word 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x30C0 0x30C0 0x1980 0x1980 0x0F00 0x0F00 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_Z: .word 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0060 0x0060 0x0060 0x00C0 0x0180 0x0300 0x0600 0x0C00 0x1800 0x3000 0x6000 0x6000 0x6000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_bracketleft: .word 0x0000 0x0000 0x0000 0x0000 0x1F00 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1800 0x1F00 0x0000 0x0000 0x0000 0x0000 0x0000
    char_backslash: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x3000 0x3000 0x1800 0x1800 0x0C00 0x0C00 0x0600 0x0600 0x0300 0x0300 0x0180 0x0180 0x00C0 0x00C0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_bracketright: .word 0x0000 0x0000 0x0000 0x0000 0x1F00 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x0300 0x1F00 0x0000 0x0000 0x0000 0x0000 0x0000
    char_asciicircum: .word 0x0000 0x0000 0x0600 0x0F00 0x1980 0x30C0 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_underscore: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0000 0x0000 0x0000
    char_grave: .word 0x1800 0x0C00 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_a: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x3F80 0x00C0 0x0060 0x0060 0x1FE0 0x3060 0x6060 0x6060 0x6060 0x3060 0x1FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_b: .word 0x0000 0x0000 0x0000 0x0000 0x6000 0x6000 0x6000 0x6000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_c: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6000 0x6000 0x6000 0x6000 0x6000 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_d: .word 0x0000 0x0000 0x0000 0x0000 0x0060 0x0060 0x0060 0x0060 0x1FE0 0x3060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x3060 0x1FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_e: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x7FE0 0x6000 0x6000 0x6000 0x3060 0x1FC0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_f: .word 0x0000 0x0000 0x0000 0x0000 0x03E0 0x0600 0x0600 0x0600 0x3FC0 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_g: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x1FE0 0x3060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x30E0 0x1FE0 0x0060 0x0060 0x00C0 0x3F80 0x0000
    char_h: .word 0x0000 0x0000 0x0000 0x0000 0x6000 0x6000 0x6000 0x6000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_i: .word 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0000 0x1E00 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_j: .word 0x0000 0x0000 0x0000 0x0000 0x00C0 0x00C0 0x00C0 0x0000 0x03C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x00C0 0x30C0 0x30C0 0x1980 0x0F00 0x0000
    char_k: .word 0x0000 0x0000 0x0000 0x0000 0x3000 0x3000 0x3000 0x3000 0x3060 0x30C0 0x3180 0x3300 0x3600 0x3C00 0x3600 0x3300 0x3180 0x30C0 0x3060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_l: .word 0x0000 0x0000 0x0000 0x0000 0x1E00 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_m: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7F80 0x66C0 0x6660 0x6660 0x6660 0x6660 0x6660 0x6660 0x6660 0x6660 0x6660 0x0000 0x0000 0x0000 0x0000 0x0000
    char_n: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_o: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x1F80 0x30C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x30C0 0x1F80 0x0000 0x0000 0x0000 0x0000 0x0000
    char_p: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7F80 0x60C0 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x60C0 0x7F80 0x6000 0x6000 0x6000 0x6000 0x0000
    char_q: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x1FE0 0x3060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x3060 0x1FE0 0x0060 0x0060 0x0060 0x0060 0x0000
    char_r: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x67E0 0x6C00 0x7800 0x7000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x6000 0x0000 0x0000 0x0000 0x0000 0x0000
    char_s: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x3FC0 0x6060 0x6000 0x6000 0x6000 0x3FC0 0x0060 0x0060 0x0060 0x6060 0x3FC0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_t: .word 0x0000 0x0000 0x0000 0x0000 0x0C00 0x0C00 0x0C00 0x0C00 0x7F80 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x07C0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_u: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x3060 0x1FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_v: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x30C0 0x30C0 0x1980 0x1980 0x0F00 0x0F00 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_w: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x6660 0x6660 0x6660 0x6660 0x6660 0x6660 0x3FC0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_x: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x30C0 0x1980 0x0F00 0x0600 0x0F00 0x1980 0x30C0 0x6060 0x6060 0x0000 0x0000 0x0000 0x0000 0x0000
    char_y: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x6060 0x30E0 0x1FE0 0x0060 0x0060 0x00C0 0x3F80 0x0000
    char_z: .word 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x7FE0 0x0060 0x00C0 0x0180 0x0300 0x0600 0x0C00 0x1800 0x3000 0x6000 0x7FE0 0x0000 0x0000 0x0000 0x0000 0x0000
    char_braceleft: .word 0x0000 0x0000 0x0000 0x0000 0x0380 0x0600 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x3800 0x0C00 0x0C00 0x0C00 0x0C00 0x0C00 0x0600 0x0380 0x0000 0x0000 0x0000 0x0000 0x0000
    char_bar: .word 0x0000 0x0000 0x0000 0x0000 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0600 0x0000 0x0000 0x0000 0x0000 0x0000
    char_braceright: .word 0x0000 0x0000 0x0000 0x0000 0x3800 0x0C00 0x0600 0x0600 0x0600 0x0600 0x0600 0x0380 0x0600 0x0600 0x0600 0x0600 0x0600 0x0C00 0x3800 0x0000 0x0000 0x0000 0x0000 0x0000
    char_asciitilde: .word 0x0000 0x0000 0x3C60 0x6660 0x6660 0x63C0 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000

# Maps ASCII codes to character bitmap addresses.
font_table:
    .word 0:32 # ASCII C0 control characters
    .word 0 # Space (draw nothing)
    .word char_exclam
    .word char_quotedbl
    .word char_numbersign
    .word char_dollar
    .word char_percent
    .word char_ampersand
    .word char_quotesingle
    .word char_parenleft
    .word char_parenright
    .word char_asterisk
    .word char_plus
    .word char_comma
    .word char_hyphen
    .word char_period
    .word char_slash
    .word char_zero
    .word char_one
    .word char_two
    .word char_three
    .word char_four
    .word char_five
    .word char_six
    .word char_seven
    .word char_eight
    .word char_nine
    .word char_colon
    .word char_semicolon
    .word char_less
    .word char_equal
    .word char_greater
    .word char_question
    .word char_at
    .word char_A
    .word char_B
    .word char_C
    .word char_D
    .word char_E
    .word char_F
    .word char_G
    .word char_H
    .word char_I
    .word char_J
    .word char_K
    .word char_L
    .word char_M
    .word char_N
    .word char_O
    .word char_P
    .word char_Q
    .word char_R
    .word char_S
    .word char_T
    .word char_U
    .word char_V
    .word char_W
    .word char_X
    .word char_Y
    .word char_Z
    .word char_bracketleft
    .word char_backslash
    .word char_bracketright
    .word char_asciicircum
    .word char_underscore
    .word char_grave
    .word char_a
    .word char_b
    .word char_c
    .word char_d
    .word char_e
    .word char_f
    .word char_g
    .word char_h
    .word char_i
    .word char_j
    .word char_k
    .word char_l
    .word char_m
    .word char_n
    .word char_o
    .word char_p
    .word char_q
    .word char_r
    .word char_s
    .word char_t
    .word char_u
    .word char_v
    .word char_w
    .word char_x
    .word char_y
    .word char_z
    .word char_braceleft
    .word char_bar
    .word char_braceright
    .word char_asciitilde
    .word 0 # DEL (draw nothing)

# MARK: Main

.text

.globl main
main:
    # Clear display by drawing an all white rectangle the size of the framebuffer
    li $a0, 0 # x
    li $a1, 0 # y
    li $a2, FRAMEBUFFER_WIDTH # w
    li $a3, FRAMEBUFFER_HEIGHT # h
    addiu $sp, $sp, -20
    li $t0, COLOR_WHITE # color (white)
    sw $t0, 16($sp)
    jal gfx_draw_rect
    addiu $sp, $sp, 20

    ##########################
    # REFERENCE TILE DRAWING #
    ##########################

    # Outer gray rectangle
    li $a0, 10 # x
    li $a1, 10 # y
    li $a2, TILE_SIZE # w
    li $a3, TILE_SIZE # h
    addiu $sp, $sp, -20
    li $t0, COLOR_DARK_GRAY # color (gray)
    sw $t0, 16($sp)
    jal gfx_draw_rect
    addiu $sp, $sp, 20

    # Inner white rectangle
    li $a0, 12 # x + 2
    li $a1, 12 # y + 2
    li $a2, TILE_INNER_SIZE # TILE_SIZE - 4
    li $a3, TILE_INNER_SIZE # TILE_SIZE - 4
    addiu $sp, $sp, -20
    li $t0, COLOR_WHITE
    sw $t0, 16($sp)
    jal gfx_draw_rect
    addiu $sp, $sp, 20

    # Letter
    li $a0, 65 # 'A' in ascii
    li $a1, 18 # x + 8
    li $a2, 12 # y + 2
    li $a3, COLOR_BLACK # color
    jal gfx_draw_char

    j sys_exit

# MARK: Syscalls

# syscall 10: Exit
sys_exit:
    li $v0, 10
    syscall
    jr $ra

# syscall 11: Print Character
sys_print_char:
    li $v0, 11
    syscall
    jr $ra

# syscall 42: random int range
sys_rand_range:
    li $v0, 42 # random int syscall
    syscall # V0 gets random number
    jr $ra

# MARK: Graphics

# Draws a filled rectangle at (x, y) with given width, height, and color.
#
# Arguments:
#   $a0: x (in pixels)
#   $a1: y (in pixels)
#   $a2: width (in pixels)
#   $a3: height (in pixels)
#   color is passed on the stack at 16($sp)
#
# Usage:
#   gfx_draw_rect(x, y, width, height, color)
gfx_draw_rect:
    beq $a2, $zero, rect_rtn # zero width → nothing to draw
    beq $a3, $zero, rect_rtn # zero height → nothing to draw

    lw $t0, 16($sp) # load color argument from the stack

    la $t1, framebuffer  # base addr of framebuffer

    # Compute x2 = x + width
    add $t5, $a0, $a2

    # Compute y2 = y + height
    add $t6, $a1, $a3

    # Convert x, x2 into byte offsets: x * 4
    sll $a0, $a0, 2 # x1_scaled
    sll $t5, $t5, 2 # x2_scaled

    # Convert y, y2 into byte offsets: y * (512 * 4) = y << 11
    sll $a1, $a1, FRAMEBUFFER_STRIDE_SHIFT # y1_scaled
    sll $t6, $t6, FRAMEBUFFER_STRIDE_SHIFT # y2_scaled

    # Compute starting ($a1) and ending ($t6) addresses
    addu $t2, $t1, $a1 # base + y1_scaled
    addu $a1, $t2, $a0 # first row, left pixel

    addu $t6, $t6, $t1 # base + y2_scaled
    addu $t6, $t6, $a0 # last row, left pixel of last row

    addu $t2, $t2, $t5 # first row, right pixel end

    # Draw the thing
    y_loop:
        move $t3, $a1 # t3 = pointer to left pixel of current row

        x_loop:
            sw $t0, ($t3) # write color to framebuffer
            addiu $t3, $t3, 4 # next pixel
            bne $t3, $t2, x_loop

        addiu $a1, $a1, FRAMEBUFFER_STRIDE # Move downward one row (left edge)
        addiu $t2, $t2, FRAMEBUFFER_STRIDE # Move downward one row (right edge)
        bne $a1, $t6, y_loop # next column

    rect_rtn:
        jr $ra

# Draws a monospaced character bitmap at (x, y) with the given color. mm 
#
# Arguments:
#   $a0: character (ASCII)
#   $a1: x (in pixels)
#   $a2: y (in pixels)
#   $a3: color (in 0x00RRGGBB format)
#
# Usage:
#   gfx_draw_char(character, x, y, color)
gfx_draw_char:
    sll $a0, $a0, 2 # offset on the font table in bytes

    la $t0, framebuffer # base addr of framebuffer

    # Load character bitmap address from font table into $t1
    la $t1, font_table # base address of font table
    add $t1, $t1, $a0 # address of font entry
    lw $t1, ($t1) # address of character bitmap
    
    beqz $t1, char_rtn # if character is not in the font table, don't draw it.

    # Convert x and y into byte offsets: x * 4, y * FRAMEBUFFER_STRIDE
    sll $a1, $a1, 2 # x_scaled
    sll $a2, $a2, FRAMEBUFFER_STRIDE_SHIFT # y_scaled

    # Compute starting address for the current row ($a2).
    addu $a2, $a2, $t0 # base + y_scaled
    addu $a2, $a2, $a1 # + x_scaled

    li $t2, 24 # remaining rows

    row_loop:
        beq $t2, $zero, char_rtn

        # Load one 16-bit row bitmap
        lh $t3, ($t1) # bitmap mask for this row
        addiu $t1, $t1, 4 # next bitmap row
        li $t5, 16 # remaining pixels on row

        move $t4, $a2 # t4 = write pointer for this row

        bit_loop:
            beq $t5, $zero, next_row

            # Extract MSB (bit 15). If set → draw pixel
            andi $t6, $t3, 0x8000
            beq $t6, $zero, next_bit # don't draw pixel if bit is not set

            sw $a3, ($t4) # draw color

        next_bit:
            # Advance bitmap and framebuffer
            sll $t3, $t3, 1 # next bit to MSB
            addiu $t4, $t4, 4 # advance to next pixel

            addiu $t5, $t5, -1 # remaining_pixels -= 1
            b bit_loop

        next_row:
            addiu $t2, $t2, -1 # remaining_rows -= 1
            addu $a2, $a2, FRAMEBUFFER_STRIDE

            b row_loop

    char_rtn:
        jr $ra

# Draws a null-terminated ASCII-encoded string of text to the given coordinate with the specified color.
#
# Arguments:
#   $a0: pointer to string
#   $a1: x
#   $a2: y
#   $a3: color
#
# Usage:
#   gfx_draw_string(string_ptr, x, y, color)
gfx_draw_string:
    str_loop:
        lbu $t0, ($a0) # load character from string pointer
        beqz $t0, str_rtn # stop once we hit a null terminator

        # Save arguments and ra
        addiu $sp, $sp, -20
        sw $ra, 16($sp)
        sw $a3, 12($sp)
        sw $a2, 8($sp)
        sw $a1, 4($sp)
        sw $a0, 0($sp)

        move $a0, $t0 # character is the first argument here
        jal gfx_draw_char
         
        # Restore arguments
        lw $a0, 0($sp)
        lw $a1, 4($sp)
        lw $a2, 8($sp)
        lw $a3, 12($sp)
        lw $ra, 16($sp)
        addiu $sp, $sp, 20

        addiu $a0, $a0, 1 # advance to next character
        addiu $a1, $a1, 16 # advance x by character width (16 pixels)

        j str_loop

    str_rtn:
        jr $ra

gfx_draw_tile:
    # TODO

    tile_rtn:
        jr $ra

gfx_draw_board:
    # top part
    li $a0, 50       # x
    li $a1, 50       # y
    li $a2, 250      # width
    li $a3, 10       # height
    addiu $sp, $sp, -20
    li $t8, 0x0000FF00    # color (green)
    sw $t8, 16($sp)
    jal gfx_draw_rect
    addiu $sp, $sp, 20

    li $t0, 6          # number of rectangles
    li $t3, 50         # y
    li $t4, 10         # width
    li $t5, 50         # height
    li $t6, 50         # x increment
    li $t7, 0	  # other loop counter

    down_loop:
        li $t1, 0         # loop counter
        li $t2, 50        # starting x

    across_loop:
        move $a0, $t2      # x
        move $a1, $t3      # y
        move $a2, $t4      # width
        move $a3, $t5      # height

        addi $sp, $sp, -20
        li $t8, 0x0000FF00    # color (green)
        sw $t8, 16($sp)
        jal gfx_draw_rect
        addiu $sp, $sp, 20

        add $t2, $t2, $t6  # x += 50
        addiu $t1, $t1, 1
        bne $t1, $t0, across_loop

        #bottom part
        li $a0,50       # x
        add $a1, $t3, $t5       # y
        li $a2, 260        # width
        li $a3, 10        # height

        # Save color argument and draw rectangle
        addiu $sp, $sp, -20
        li $t8, 0x0000FF00    # color (green)
        sw $t8, 16($sp)
        jal gfx_draw_rect
        addiu $sp, $sp, 20

        add $t3, $t3, $t5
        addiu $t7, $t7, 1
        bne $t7, $t0, down_loop

gfx_draw_frame:
    # call gfx_draw_board
    addiu $sp, $sp, -4
    sw $ra, ($sp)
    jal gfx_draw_board
    lw $ra, ($sp)
    addiu $sp, $sp, 4

    jr $ra