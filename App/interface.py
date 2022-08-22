import streamlit as st
import SessionState
import utilities


session_state = SessionState.get(button_start=False, button_submit_mood=False,button_submit_preferences=False, button_submit_sugg_kind=False, button_submit_genres=False, button_check_artist_information=False, button_check_track_information=False, sugg_kind=False, colonna_scelta='Seleziona')

happiness = ('Sad', 'Flat', 'Happy')
energy_values = ('Tired', 'Normal', 'Energic')
danceability = ('No', 'Maybe...', 'YES!')
page_names = ('Mood', 'Preferences')
suggest = ('Artists', 'Tracks')
genres=('hip hop','pop','jazz','rock','country','rap','soul','folk','classical','metal','funky','indie','house','punk','electronic','reggae','latin','songwriter','children','soundtrack', 'relax')

st.set_page_config(page_title = "MusicRec",page_icon = "🔎")


st.markdown("<h1 style='text-align: center; color: black;'>Welcome to </h1>"
                                            "<h1 style='text-align: center; color: red;'>MusicRec!</h1>",
                                            unsafe_allow_html=True)

left_0,center_0,right_0=st.beta_columns([4,11,10])
with center_0:
    st.markdown("![Alt Text](https://media.giphy.com/media/tqfS3mgQU28ko/giphy.gif)",
                                                unsafe_allow_html=True)

st.markdown("<h2 style='text-align: center; color: black;'>Ready to dive into music with me? 🙌 </h2>",
            unsafe_allow_html=True)


left_1, center_1, right_1 =st.beta_columns([10,2,10])
with center_1:
    start_button=st.button('Start')

if start_button:
    session_state.button_start = True


if session_state.button_start:
    st.title("How are you feeling today?")
    valence = st.select_slider("Are you happy?",happiness)
    energy=st.select_slider('Are you energic?',energy_values)
    dance=st.select_slider('Are you in the mood for dancing?', danceability)
    submit_mood = st.button("Submit Mood")
    if submit_mood:
        session_state.valence = valence
        session_state.energy = energy
        session_state.dance = dance
        st.markdown(f'**Your mood is**' + ' '+str(session_state.valence)+' '+str(session_state.energy)+' '+str(session_state.dance))
        session_state.button_submit_mood = True
        session_state.suggestions_features = utilities.return_tracks([session_state.valence, session_state.energy, session_state.dance])


cache_genres=[]
if session_state.button_submit_mood:
    st.title('Give me an idea of your musical tastes...')
    genres_pref = st.multiselect('What genres do you like?', genres)
    submit_genres=st.button('Submit Genres')
    if submit_genres:
        for i in genres_pref:
            cache_genres.append(i)
        session_state.button_submit_genres = True
        session_state.genres=cache_genres
        print(session_state.genres)
        session_state.suggestions_genres = utilities.suggestTracksByGenre(session_state.genres)

cache_ids=[]
if session_state.button_submit_genres:
    st.title('Now some suggestions will be shown')
    retry = st.button('Load other suggestions')
    if retry:
        session_state.suggestions_features = utilities.return_tracks([session_state.valence, session_state.energy, session_state.dance])
        session_state.suggestions_genres = utilities.suggestTracksByGenre(session_state.genres)
    session_state.suggestion_tot = session_state.suggestions_features | session_state.suggestions_genres
    print(session_state.suggestion_tot)
    preferences = st.multiselect('Select the songs that you like', list(session_state.suggestion_tot.keys()))
    st.title('Do you like any song among these?')
    submit_preferences = st.button("Submit Preferences")
    if submit_preferences:
        for i in preferences:
            print(session_state.suggestion_tot.get(i))
            cache_ids.append(session_state.suggestion_tot.get(i))
        print(cache_ids)
        session_state.preferences_ids = cache_ids
        st.markdown('**Your preferences: **')
        for i in preferences:
            st.write(str(i))
        session_state.preferences = preferences
        session_state.button_submit_preferences = True


    if session_state.button_submit_preferences:
        st.title('What kind of suggestion would you like? ')
        sugg_kind = st.radio('Artist or tracks?', suggest)
        submit_sugg_kind= st.button('Submit kind of suggestion')
        if submit_sugg_kind:
            st.write(f'You want a: {sugg_kind} suggestion')
            session_state.button_submit_sugg_kind = True
            session_state.sugg_kind = sugg_kind
            st.title('First suggestion basing on what you liked...')

            cache_artists = []
            session_state.results_artist = utilities.suggestionArtists(session_state.preferences_ids)
            session_state.results_tracks = utilities.suggestionsTracks(session_state.preferences_ids)
        if session_state.sugg_kind == 'Artists':
            artist_information = st.radio('Choose to see artist information', session_state.results_artist)
            check_artist_information = st.button("See artist information")
            if check_artist_information:
                session_state.button_check_artist_information = True
                st.write(f'Retrieving information about: {artist_information}')
                session_state.check_artist_information = "[\""+artist_information+"\"]"
                if session_state.button_check_artist_information:
                    session_state.artists_information = utilities.return_albums_by_artist(session_state.check_artist_information)
                    st.write("The albums that this artist published are:")
                    for i in session_state.artists_information:
                        st.markdown("- " + i.title())




        if session_state.sugg_kind == 'Tracks':
            track_information = st.radio('Choose to see track information', list(session_state.results_tracks))
            check_track_information = st.button("See track information")
            if check_track_information:
                session_state.button_check_track_information = True
                st.write(f'Retrieving information about: {track_information}')
                if track_information in session_state.results_tracks.keys():
                    id=session_state.results_tracks.get(track_information)
                session_state.check_track_information = "[\""+id+"\"]"
                session_state.track_information=utilities.return_albums_by_track(session_state.check_track_information)
                st.write("The album where the song comes from is:")
                st.write(session_state.track_information.title())
