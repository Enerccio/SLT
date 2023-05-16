#!/bin/bash

rm distributions/*

echo -n Private key password:
read -s PASSWORD
export PRIVATE_KEY_PASSWORD=$PASSWORD

expandedName() (
    # shellcheck disable=SC2125
    UNEXPANDED=build/distributions/slt-signed.zip
    set +f
    echo $UNEXPANDED
)

DISTRIBUTIONS="IC"
for TARGET_IDE in ${DISTRIBUTIONS}; do
    echo Trying to build $TARGET_IDE
    export TARGET_IDE=$TARGET_IDE
    ./gradlew clean signPlugin verifyPlugin buildPlugin || ./gradlew signPlugin verifyPlugin buildPlugin || exit
    FILE=$(expandedName)
    NEW_FILE=$(basename ${FILE%.*}-$TARGET_IDE.zip)
    cp "$FILE" distributions/$NEW_FILE
    echo Built $NEW_FILE
done
