module MonsterAttack exposing (..)


type alias MonsterDamage =
    String


attackWith : String -> Int -> MonsterDamage -> MonsterDamage
attackWith weapon strength monsterDamage =
    monsterDamage ++ "Attacked with " ++ weapon ++ " of strength " ++ String.fromInt strength ++ "."


attackWithSword1 : MonsterDamage -> Int -> MonsterDamage
attackWithSword1 monsterDamage strength =
    attackWith "sword" strength monsterDamage


attackWithClaw1 : MonsterDamage -> Int -> MonsterDamage
attackWithClaw1 monsterDamage strength =
    attackWith "claw" strength monsterDamage


attack1 : MonsterDamage -> MonsterDamage
attack1 monsterDamage =
    attackWithSword1 monsterDamage 5
        ++ attackWithClaw1 "" 1
        ++ attackWithClaw1 "" 1
        ++ attackWithSword1 "" 5


attackWithSword2 : Int -> MonsterDamage -> MonsterDamage
attackWithSword2 strength monsterDamage =
    attackWith "sword" strength monsterDamage


attackWithClaw2 : Int -> MonsterDamage -> MonsterDamage
attackWithClaw2 strength monsterDamage =
    attackWith "claw" strength monsterDamage


attack2 : MonsterDamage -> MonsterDamage
attack2 monsterDamage =
    monsterDamage
        |> attackWithSword2 5
        |> attackWithClaw2 1
        |> attackWithClaw2 1
        |> attackWithSword2 5


attack3 : MonsterDamage -> MonsterDamage
attack3 =
    attackWithSword2 5
        >> attackWithClaw2 1
        >> attackWithClaw2 1
        >> attackWithSword2 5
