# Functionnal Data Analysis

## Détection d'anomalie dans une collection de courbes

- Etude et implémentation de fonction profondeur pour les données fonctionnelles

    - Fonction profondeur de Tukey, fonction profondeur simpliciale

    - Application sur les courbes de charge de parcs éoliens (Enedis)

    - Détection d'anomalie par cluster pour limiter l'influence de facteurs exogènes comme la vitess du vent, etc.

- Etude et implémentation de la détection d'anomalie sur les résidus d'un modèle linéaire fonctionnel

    - Objectif : extraire l'influence du vent sur la production électrique des éoliennes pour une meilleure detection d'anomalie

    - Etude du modèle linéaire fonctionnel
        - **variable dépendante :** courbes de charge
        - **variable indépendant :** courbes de vistesse du vent à 100 mètres

    - Extraction des résidus et application de la détection d'anomalie
        - Application sur toute la collection de courbes
        - Application par cluster de courbes homogènes
