#!/bin/env bash

# https://partner.steamgames.com/doc/store/getreviews?l=french&language=english

# https://store.steampowered.com/app/1736630/Minesweeper_Ultimate/
app_id=1736630

#wget https://store.steampowered.com/appreviews/${app_id}?json=1&filter=all
wget -O reviews.json "https://store.steampowered.com/appreviews/${app_id}?json=9&filter=all&language=english&cursor=*&day_range=360&num_per_page=99"
#wget -O reviews2.json "https://store.steampowered.com/appreviews/${app_id}?json=9&filter=all&language=all&cursor=AoIIPwPhAH6H4%2fAE"

#"cursor": "AoIIPwPhAH6H4/AE",
